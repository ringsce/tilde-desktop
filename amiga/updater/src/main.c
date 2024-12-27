#include "gui.h"
#include "torrent.h"

int main(int argc, char **argv) {
    if (!init_gui(argc, argv)) {
        return 1;
    }

    init_torrent();

    run_gui();

    cleanup_torrent();
    cleanup_gui();

    return 0;
}
