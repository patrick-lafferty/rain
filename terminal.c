#include <sys/ioctl.h>

//gcc -shared -o libterminal.so -fPIC terminal.c

int getTerminalWidth()
{
    struct winsize ws;
    ioctl(0, TIOCGWINSZ, &ws);

    return ws.ws_col;
}