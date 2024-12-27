#include <exec/types.h>
#include <intuition/intuition.h>
#include <proto/exec.h>
#include <proto/intuition.h>

#define WINDOW_TITLE "AROS Window Example"
#define WINDOW_WIDTH 400
#define WINDOW_HEIGHT 300

int main() {
    struct Window *win;
    struct IntuiMessage *msg;
    BOOL running = TRUE;

    // Open a simple window
    win = OpenWindowTags(NULL,
        WA_Title, (ULONG)WINDOW_TITLE,
        WA_Width, WINDOW_WIDTH,
        WA_Height, WINDOW_HEIGHT,
        WA_Flags, WFLG_CLOSEGADGET | WFLG_DEPTHGADGET | WFLG_SIZEGADGET | WFLG_DRAGBAR,
        WA_IDCMP, IDCMP_CLOSEWINDOW | IDCMP_RAWKEY,
        TAG_DONE);

    if (!win) {
        Printf("Failed to open window.\n");
        return RETURN_FAIL;
    }

    Printf("Window opened successfully.\n");

    // Main event loop
    while (running) {
        Wait(1L << win->UserPort->mp_SigBit);

        // Process events
        while ((msg = (struct IntuiMessage *)GetMsg(win->UserPort)) != NULL) {
            ULONG class = msg->Class;
            UWORD code = msg->Code;
            ReplyMsg((struct Message *)msg);

            switch (class) {
                case IDCMP_CLOSEWINDOW:
                    running = FALSE;
                    break;

                case IDCMP_RAWKEY:
                    // Handle keyboard input
                    Printf("Key pressed: 0x%X\n", code);
                    break;

                default:
                    break;
            }
        }
    }

    // Clean up
    CloseWindow(win);
    Printf("Window closed.\n");

    return RETURN_OK;
}
