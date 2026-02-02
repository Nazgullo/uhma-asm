#!/usr/bin/env python3
"""
export_weights.py — Export MPNet weights for assembly embedding

Exports all-mpnet-base-v2 weights to raw binary files that can be
memory-mapped directly by the assembly implementation.

Output structure:
  weights/
  ├── config.bin          # Model config (dimensions, layers, etc.)
  ├── vocab.bin           # BPE vocabulary (token_id → string)
  ├── merges.bin          # BPE merge rules
  ├── embeddings.bin      # Token embeddings (30K × 768 × f32)
  ├── position.bin        # Position embeddings (512 × 768 × f32)
  ├── layer_00/           # Per-layer weights
  │   ├── attn_qkv.bin    # Combined Q,K,V projection (768 × 2304 × f32)
  │   ├── attn_out.bin    # Attention output (768 × 768 × f32)
  │   ├── ln1_weight.bin  # LayerNorm 1 gamma (768 × f32)
  │   ├── ln1_bias.bin    # LayerNorm 1 beta (768 × f32)
  │   ├── ffn_up.bin      # FFN up-project (768 × 3072 × f32)
  │   ├── ffn_down.bin    # FFN down-project (3072 × 768 × f32)
  │   ├── ln2_weight.bin  # LayerNorm 2 gamma (768 × f32)
  │   └── ln2_bias.bin    # LayerNorm 2 beta (768 × f32)
  ├── ... (layers 01-11)
  ├── final_ln_weight.bin # Final LayerNorm gamma
  ├── final_ln_bias.bin   # Final LayerNorm beta
  └── projection.bin      # 768 → 8192 projection (trained or random orthogonal)

Usage:
  python3 export_weights.py [--output-dir weights/] [--projection random|trained]
"""

import os
import sys
import struct
import json
import numpy as np
from pathlib import Path

def ensure_dir(path):
    Path(path).mkdir(parents=True, exist_ok=True)

def save_f32(arr, path):
    """Save numpy array as raw f32 binary."""
    arr = arr.astype(np.float32).flatten()
    with open(path, 'wb') as f:
        f.write(arr.tobytes())
    print(f"  Saved {path}: {arr.shape[0]} floats ({arr.nbytes / 1024 / 1024:.2f} MB)")

def save_config(config, path):
    """Save config as binary struct."""
    # Config format: all u32
    # [0] vocab_size
    # [1] hidden_size
    # [2] num_layers
    # [3] num_heads
    # [4] intermediate_size
    # [5] max_position
    # [6] projection_dim
    data = struct.pack('<7I',
        config['vocab_size'],
        config['hidden_size'],
        config['num_hidden_layers'],
        config['num_attention_heads'],
        config['intermediate_size'],
        config['max_position_embeddings'],
        8192  # projection dim for UHMA
    )
    with open(path, 'wb') as f:
        f.write(data)
    print(f"  Saved {path}: config struct")

def save_vocab(tokenizer, path):
    """Save vocabulary as binary: [num_tokens:u32] [offset:u32]* [strings]"""
    vocab = tokenizer.get_vocab()
    sorted_vocab = sorted(vocab.items(), key=lambda x: x[1])

    # Build string table
    strings = b''
    offsets = []
    for token, idx in sorted_vocab:
        offsets.append(len(strings))
        encoded = token.encode('utf-8', errors='replace')
        strings += struct.pack('<H', len(encoded)) + encoded

    # Write header + offsets + strings
    with open(path, 'wb') as f:
        f.write(struct.pack('<I', len(sorted_vocab)))
        for off in offsets:
            f.write(struct.pack('<I', off))
        f.write(strings)

    print(f"  Saved {path}: {len(sorted_vocab)} tokens")

def create_orthogonal_projection(in_dim, out_dim, seed=42):
    """Create random orthogonal projection matrix."""
    rng = np.random.default_rng(seed)
    # Generate random matrix and orthogonalize via QR
    A = rng.standard_normal((out_dim, in_dim)).astype(np.float32)
    Q, R = np.linalg.qr(A.T)
    # Q is in_dim × min(in_dim, out_dim), we need in_dim × out_dim
    if out_dim > in_dim:
        # Pad with more orthogonal vectors
        extra = rng.standard_normal((in_dim, out_dim - in_dim)).astype(np.float32)
        Q = np.hstack([Q, extra])
    return Q[:, :out_dim].T  # out_dim × in_dim

def export_mpnet(output_dir='weights', projection_type='random'):
    """Export all-mpnet-base-v2 weights."""
    try:
        from transformers import AutoModel, AutoTokenizer, AutoConfig
    except ImportError:
        print("Error: transformers not installed. Run: pip install transformers torch")
        sys.exit(1)

    print("Loading all-mpnet-base-v2...")
    model_name = "sentence-transformers/all-mpnet-base-v2"

    config = AutoConfig.from_pretrained(model_name)
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModel.from_pretrained(model_name)
    model.eval()

    print(f"\nModel config:")
    print(f"  vocab_size: {config.vocab_size}")
    print(f"  hidden_size: {config.hidden_size}")
    print(f"  num_layers: {config.num_hidden_layers}")
    print(f"  num_heads: {config.num_attention_heads}")
    print(f"  intermediate_size: {config.intermediate_size}")
    print(f"  max_position: {config.max_position_embeddings}")

    ensure_dir(output_dir)

    # Save config
    print("\nExporting config...")
    save_config({
        'vocab_size': config.vocab_size,
        'hidden_size': config.hidden_size,
        'num_hidden_layers': config.num_hidden_layers,
        'num_attention_heads': config.num_attention_heads,
        'intermediate_size': config.intermediate_size,
        'max_position_embeddings': config.max_position_embeddings,
    }, f"{output_dir}/config.bin")

    # Save vocab
    print("\nExporting vocabulary...")
    save_vocab(tokenizer, f"{output_dir}/vocab.bin")

    # Save embeddings
    print("\nExporting embeddings...")
    state = model.state_dict()

    # Word embeddings
    word_emb = state['embeddings.word_embeddings.weight'].numpy()
    save_f32(word_emb, f"{output_dir}/embeddings.bin")

    # Position embeddings
    pos_emb = state['embeddings.position_embeddings.weight'].numpy()
    save_f32(pos_emb, f"{output_dir}/position.bin")

    # LayerNorm after embeddings
    save_f32(state['embeddings.LayerNorm.weight'].numpy(), f"{output_dir}/emb_ln_weight.bin")
    save_f32(state['embeddings.LayerNorm.bias'].numpy(), f"{output_dir}/emb_ln_bias.bin")

    # Export relative attention bias (MPNet specific)
    print("\nExporting relative attention bias...")
    save_f32(state['encoder.relative_attention_bias.weight'].numpy(),
             f"{output_dir}/relative_attn_bias.bin")

    # Export each transformer layer
    print("\nExporting transformer layers...")
    for layer_idx in range(config.num_hidden_layers):
        layer_dir = f"{output_dir}/layer_{layer_idx:02d}"
        ensure_dir(layer_dir)
        print(f"  Layer {layer_idx}:")

        prefix = f'encoder.layer.{layer_idx}'

        # Attention Q, K, V weights - combine into single matrix for efficiency
        # MPNet uses attention.attn.{q,k,v} not attention.self.{query,key,value}
        q_w = state[f'{prefix}.attention.attn.q.weight'].numpy()
        k_w = state[f'{prefix}.attention.attn.k.weight'].numpy()
        v_w = state[f'{prefix}.attention.attn.v.weight'].numpy()
        qkv_w = np.concatenate([q_w, k_w, v_w], axis=0)  # (3*768, 768)
        save_f32(qkv_w, f"{layer_dir}/attn_qkv_weight.bin")

        q_b = state[f'{prefix}.attention.attn.q.bias'].numpy()
        k_b = state[f'{prefix}.attention.attn.k.bias'].numpy()
        v_b = state[f'{prefix}.attention.attn.v.bias'].numpy()
        qkv_b = np.concatenate([q_b, k_b, v_b], axis=0)
        save_f32(qkv_b, f"{layer_dir}/attn_qkv_bias.bin")

        # Attention output projection (MPNet uses attention.attn.o)
        save_f32(state[f'{prefix}.attention.attn.o.weight'].numpy(),
                 f"{layer_dir}/attn_out_weight.bin")
        save_f32(state[f'{prefix}.attention.attn.o.bias'].numpy(),
                 f"{layer_dir}/attn_out_bias.bin")

        # Attention LayerNorm (MPNet uses attention.LayerNorm)
        save_f32(state[f'{prefix}.attention.LayerNorm.weight'].numpy(),
                 f"{layer_dir}/attn_ln_weight.bin")
        save_f32(state[f'{prefix}.attention.LayerNorm.bias'].numpy(),
                 f"{layer_dir}/attn_ln_bias.bin")

        # FFN
        save_f32(state[f'{prefix}.intermediate.dense.weight'].numpy(),
                 f"{layer_dir}/ffn_up_weight.bin")
        save_f32(state[f'{prefix}.intermediate.dense.bias'].numpy(),
                 f"{layer_dir}/ffn_up_bias.bin")
        save_f32(state[f'{prefix}.output.dense.weight'].numpy(),
                 f"{layer_dir}/ffn_down_weight.bin")
        save_f32(state[f'{prefix}.output.dense.bias'].numpy(),
                 f"{layer_dir}/ffn_down_bias.bin")

        # FFN LayerNorm
        save_f32(state[f'{prefix}.output.LayerNorm.weight'].numpy(),
                 f"{layer_dir}/ffn_ln_weight.bin")
        save_f32(state[f'{prefix}.output.LayerNorm.bias'].numpy(),
                 f"{layer_dir}/ffn_ln_bias.bin")

    # Pooler (optional - we use mean pooling instead)
    # save_f32(state['pooler.dense.weight'].numpy(), f"{output_dir}/pooler_weight.bin")
    # save_f32(state['pooler.dense.bias'].numpy(), f"{output_dir}/pooler_bias.bin")

    # Projection layer: 768 → 8192
    print("\nCreating projection layer (768 → 8192)...")
    if projection_type == 'random':
        proj = create_orthogonal_projection(768, 8192)
        print(f"  Created random orthogonal projection")
    else:
        # TODO: Load trained projection if available
        proj = create_orthogonal_projection(768, 8192)
        print(f"  Using random orthogonal projection (trained not available)")

    save_f32(proj, f"{output_dir}/projection.bin")

    # Summary
    total_size = sum(f.stat().st_size for f in Path(output_dir).rglob('*.bin'))
    print(f"\n{'='*50}")
    print(f"Export complete!")
    print(f"Total size: {total_size / 1024 / 1024:.2f} MB")
    print(f"Output dir: {output_dir}/")

    # Verify by loading and checking shapes
    print(f"\nVerification:")
    emb = np.fromfile(f"{output_dir}/embeddings.bin", dtype=np.float32)
    print(f"  embeddings: {emb.shape[0]} floats = {config.vocab_size} × {config.hidden_size}")
    proj = np.fromfile(f"{output_dir}/projection.bin", dtype=np.float32)
    print(f"  projection: {proj.shape[0]} floats = 8192 × 768")

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Export MPNet weights for assembly')
    parser.add_argument('--output-dir', default='weights', help='Output directory')
    parser.add_argument('--projection', default='random', choices=['random', 'trained'],
                        help='Projection type')
    args = parser.parse_args()

    export_mpnet(args.output_dir, args.projection)
