; stream_stub.asm — Stub symbols for builds without gateway streaming
;
; Provides gw_stream_client and gateway_stream_send so format.asm can link
; into tools that do not include gateway.o (e.g., tools/mcp_server).
;
; @entry gateway_stream_send(rsi=buf, edx=len) -> returns immediately
;
section .data
    gw_stream_client: dd -1

section .text
    global gw_stream_client
    global gateway_stream_send

gateway_stream_send:
    ret
