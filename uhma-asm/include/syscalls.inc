; syscalls.inc — Linux x86_64 syscall numbers and constants
%ifndef SYSCALLS_INC
%define SYSCALLS_INC

%define SYS_READ            0
%define SYS_WRITE           1
%define SYS_OPEN            2
%define SYS_CLOSE           3
%define SYS_MMAP            9
%define SYS_MPROTECT        10
%define SYS_MUNMAP          11
%define SYS_BRK             12
%define SYS_RT_SIGACTION    13
%define SYS_RT_SIGRETURN    15
%define SYS_NANOSLEEP       35
%define SYS_GETPID          39
%define SYS_EXIT            60
%define SYS_FCNTL           72
%define SYS_FTRUNCATE       77
%define SYS_GETCWD          79
%define SYS_RENAME          82
%define SYS_UNLINK          87
%define SYS_CLOCK_GETTIME   228
%define SYS_GETRANDOM       318

; mmap protection flags
%define PROT_READ           0x1
%define PROT_WRITE          0x2
%define PROT_EXEC           0x4
%define PROT_RWX            (PROT_READ | PROT_WRITE | PROT_EXEC)

; mmap flags
%define MAP_PRIVATE         0x02
%define MAP_ANONYMOUS       0x20
%define MAP_FIXED           0x10

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

; sigaction flags
%define SA_SIGINFO          0x04
%define SA_RESTORER         0x04000000

; file descriptors
%define STDIN               0
%define STDOUT              1
%define STDERR              2

%endif
