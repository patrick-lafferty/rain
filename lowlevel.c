#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>

int main()
{
    struct termios old;
    tcgetattr(0, &old);
    //printf("x: %lu\n", sizeof(int));
    printf("x: %u", old.c_lflag);
    /*printf("echo: %d", ECHO);
    printf("echoe: %d", ECHOE);
    printf("icanon: %d", ICANON);*/
    printf("~all: %u", 35387 & ~(ECHO | ECHOE | ICANON));
}

void set_stdin() {
    /*int fd = open("/dev/null", O_WRONLY);
    dup2(fd, 0);
    close(fd);
*/
printf("before: %p\n", stdin);
    int dupin = dup(0);
    close(0);
    int fd = open("/dev/tty", O_RDONLY);
    dup2(fd, 0);//, fileno(stdin));

    //stdin = fdopen(0, "rb");
    //stdin = fdopen(fd, "rb");
    //stdout = fdopen(1, "wb");
    printf("redirected them: %p %p\n", stdin, stdout);
    //fflush(stdin);
    //fflush(stdout);
}

void run() {
    int pid = fork();

    if (pid == 0) {
        //set_stdin();
        //printf("wtf %d\n", fcntl(0, F_GETFD));

        //freopen("/dev/tty", "r", stdin);

        char* argv[2];
        argv[0] = "python";
        argv[1] = NULL;
        execvp("python", argv);
        printf("errno: %d\n", errno);
        printf("%s\n", strerror(errno));
        _exit(1);
    }
    else {
        wait(NULL);
    }
}

//gcc -shared -o liblowlevel.so -fPIC lowlevel.c