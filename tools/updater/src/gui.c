#include "gui.h"
#include <aros/intuition.h>

static struct Window *main_window;

int init_gui(int argc, char **argv) {
    main_window = OpenWindowTags(NULL,
        WA_Title, (IPTR) "AROS Torrent Client",
        WA_Width, 400,
        WA_Height, 300,
        WA_Flags, WFLG_CLOSEGADGET | WFLG_SIZEGADGET,
        TAG_END);

    if (!main_window) {
        return 0;
    }

    return 1;
}

void run_gui() {
    struct IntuiMessage *msg;
    BOOL running = TRUE;

    while (running) {
        Wait(1L << main_window->UserPort->mp_SigBit);
        while ((msg = (struct IntuiMessage *) GetMsg(main_window->UserPort))) {
            if (msg->Class == IDCMP_CLOSEWINDOW) {
                running = FALSE;
            }
            ReplyMsg((struct Message *) msg);
        }
    }
}

void cleanup_gui() {
    if (main_window) {
        CloseWindow(main_window);
    }
}
