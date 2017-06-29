#include <sys/inotify.h>
//#include <sys/eventfd.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <vector>
#include <unistd.h>
#include <iostream>
#include <sys/stat.h>

//g++ -shared -o libnotify.so -fPIC notify.cpp -std=c++11

int inotify_file_descriptor {-1};
//int event_file_descriptor {-1};
int epoll {-1};

int* read_events(int* lengthh)
{
    int NAME_MAX = 255;
    int event_size = sizeof(inotify_event) + NAME_MAX + 1;
    int max_events = 10;
    int buffer_size = event_size * max_events;
    //char buffer[buffer_size];
    std::vector<unsigned char> buffer(buffer_size);

    int length = read(inotify_file_descriptor, buffer.data(), buffer_size);

    int i = 0;
    std::vector<int> wds;

    while (i < length) 
    {
        inotify_event* event = reinterpret_cast<inotify_event*>(&buffer[i]);

        if (event->len)
        {
            if (event->mask & IN_CREATE)
            {
        wds.push_back(event->wd);
                std::cout << "created " << event->name << std::endl;
            }
            else if (event->mask & IN_DELETE)
            {
        wds.push_back(event->wd);
                std::cout << "delete " << event->name << std::endl;
            }
            else if (event->mask & IN_MODIFY && !(event->mask & IN_ISDIR))
            {
                struct stat s;
                std::string path {"/home/pat/projects/lush/docs/"};
                path += event->name;
                if (stat(path.c_str(), &s) == 0 && !S_ISDIR(s.st_mode)) 
                {
                    if (event->mask & IN_ISDIR)
                    {
                        std::cout << event->name << " is a dir" << std::endl;
                    }

                    wds.push_back(event->wd);

                    std::cout << "modify " << event->name << std::endl;
                }
            }
        }

        i += event_size + event->len;
    }

    auto outbuffer = (int*)malloc(sizeof(int) * wds.size());
    std::copy(begin(wds), end(wds), outbuffer);

    *lengthh = wds.size();
    return outbuffer;
}

//extern "C" int watch_all(int* wd_buffer)
extern "C" int* watch_all(int* length)
{
    /*if (epoll < 0)
    {
        return epoll;
    }*/

    if (inotify_file_descriptor < 0)
    {
        return nullptr;//inotify_file_descriptor;
    }

/*    constexpr int MAX_EVENTS = 5;
    epoll_event events[MAX_EVENTS];

    int count = epoll_wait(epoll, events, MAX_EVENTS, -1);

    if (count < 0)
    {
        return count;
    }

    for (int i = 0; i < count; i++)
    {
        auto& event = events[i];
        std::cout << "received event" << std::endl;
    }*/
    return read_events(length); 
}

extern "C" int watch_path(char* path) 
{
    if (inotify_file_descriptor < 0) 
    {
        inotify_file_descriptor = inotify_init();

        if (inotify_file_descriptor < 0)
        {
            return inotify_file_descriptor;
        }
    }

    /*if (event_file_descriptor < 0)
    {
        event_file_descriptor = eventfd(0, 0);

        if (event_file_descriptor < 0)
        {
            return event_file_descriptor;
        }
    }*/

    /*if (epoll < 0)
    {
        epoll = epoll_create(1);

        if (epoll < 0)
        {
            return epoll;
        }
    }
*/
    int watchDescriptor = inotify_add_watch(inotify_file_descriptor,
        path,
        IN_CREATE | IN_DELETE | IN_MODIFY);

    /*epoll_event event;
    event.events = EPOLLIN;
    event.data.fd = inotify_file_descriptor;

    epoll_ctl(epoll, EPOLL_CTL_ADD, inotify_file_descriptor, &event);
*/    
}