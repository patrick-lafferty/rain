#include <sys/inotify.h>
#include <stdlib.h>
#include <vector>
#include <unistd.h>
#include <iostream>
#include <sys/stat.h>
#include <map>

//g++ -shared -o libnotify.so -fPIC notify.cpp -std=c++11

int inotify_file_descriptor {-1};

std::map<int, std::string> watchers;

int* read_events(int* lengthh)
{
    int NAME_MAX = 255;
    int event_size = sizeof(inotify_event) + NAME_MAX + 1;
    int max_events = 10;
    int buffer_size = event_size * max_events;
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
            }
            else if (event->mask & IN_DELETE)
            {
                wds.push_back(event->wd);
            }
            else if (event->mask & IN_MODIFY && !(event->mask & IN_ISDIR))
            {
                struct stat s;
                std::string path = watchers[event->wd];
                path += std::string{"/"} + event->name;

                if (stat(path.c_str(), &s) == 0 && !S_ISDIR(s.st_mode)) 
                {
                    wds.push_back(event->wd);
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

extern "C" int* watch_all(int* length)
{
    if (inotify_file_descriptor < 0)
    {
        return nullptr;
    }

    return read_events(length); 
}

extern "C" int stop_watching_path(int watch_descriptor)
{
    auto it = watchers.find(watch_descriptor);

    if (it != end(watchers))
    {
        watchers.erase(it);
    }

    return inotify_rm_watch(inotify_file_descriptor, watch_descriptor);
}

extern "C" int stop_watching()
{
    close(inotify_file_descriptor);
    inotify_file_descriptor = -1;
    return 0;
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

    int watchDescriptor = inotify_add_watch(inotify_file_descriptor,
        path,
        IN_CREATE | IN_DELETE | IN_MODIFY);

    watchers[watchDescriptor] = path;

    return watchDescriptor;
}