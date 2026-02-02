; syscalls.inc — Linux x86_64 syscall numbers and constants
%ifndef SYSCALLS_INC
%define SYSCALLS_INC

%define SYS_READ            0
%define SYS_WRITE           1
%define SYS_OPEN            2
%define SYS_CLOSE           3
%define SYS_STAT            4
%define SYS_FSTAT           5
%define SYS_LSTAT           6
%define SYS_MMAP            9
%define SYS_MPROTECT        10
%define SYS_MUNMAP          11
%define SYS_BRK             12
%define SYS_MSYNC           26
%define SYS_MADVISE         28

; madvise flags
%define MADV_NORMAL         0                 ; no special treatment
%define MADV_RANDOM         1                 ; expect random access
%define MADV_SEQUENTIAL     2                 ; expect sequential access
%define MADV_WILLNEED       3                 ; will need these pages
%define MADV_DONTNEED       4                 ; don't need these pages
%define MADV_HUGEPAGE       14                ; use huge pages if possible
%define SYS_RT_SIGACTION    13
%define SYS_RT_SIGRETURN    15
%define SYS_NANOSLEEP       35
%define SYS_GETPID          39
%define SYS_FORK            57
%define SYS_EXECVE          59
%define SYS_EXIT            60
%define SYS_FCNTL           72
%define SYS_FTRUNCATE       77
%define SYS_GETCWD          79
%define SYS_RENAME          82
%define SYS_GETTIMEOFDAY    96
%define SYS_UNLINK          87
%define SYS_LSEEK           8
%define SYS_PWRITE          18
%define SYS_MKDIR           83
%define SYS_GETDENTS        78
%define SYS_OPENAT          257
%define SYS_FSYNC           74
%define SYS_CLOCK_GETTIME   228
%define SYS_GETRANDOM       318

; mmap protection flags
%define PROT_READ           0x1
%define PROT_WRITE          0x2
%define PROT_EXEC           0x4
%define PROT_RWX            (PROT_READ | PROT_WRITE | PROT_EXEC)

; mmap flags
%define MAP_SHARED          0x01
%define MAP_PRIVATE         0x02
%define MAP_FIXED           0x10
%define MAP_ANONYMOUS       0x20

; open flags
%define O_RDONLY            0
%define O_WRONLY            1
%define O_RDWR              2
%define O_CREAT             0x40
%define O_TRUNC             0x200

; file mode
%define FILE_MODE           0o644

; signal numbers
%define SIGSEGV             11
%define SIGFPE              8
%define SIGBUS              7
%define SIGILL              4
%define SIGPIPE             13
%define SIGTRAP             5     ; INT 3 breakpoint (self-debugger)

; sigaction flags
%define SA_SIGINFO          0x04
%define SA_RESTORER         0x04000000

; file descriptors
%define STDIN               0
%define STDOUT              1
%define STDERR              2

; socket syscalls
%define SYS_SOCKET          41
%define SYS_BIND            49
%define SYS_LISTEN          50
%define SYS_ACCEPT          43
%define SYS_CONNECT         42
%define SYS_SETSOCKOPT      54
%define SYS_SELECT          23
%define SYS_POLL            7

; socket constants
%define AF_INET             2
%define SOCK_STREAM         1
%define SOL_SOCKET          1
%define SO_REUSEADDR        2
%define INADDR_LOOPBACK     0x0100007F  ; 127.0.0.1 in network byte order

; poll events
%define POLLIN              0x0001
%define POLLOUT             0x0004
%define POLLERR             0x0008
%define POLLHUP             0x0010

%endif
