#import <Cocoa/Cocoa.h>
#include "clipboard.h"
#include <string.h>
#include <stdlib.h>

void clipboard_set(const char *text) {
    if (!text) return;
    
    @autoreleasepool {
        NSString *nsText = [NSString stringWithUTF8String:text];
        NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
        [pasteboard clearContents];
        [pasteboard setString:nsText forType:NSPasteboardTypeString];
    }
}

char* clipboard_get(void) {
    @autoreleasepool {
        NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
        NSString *nsText = [pasteboard stringForType:NSPasteboardTypeString];
        
        if (!nsText) return NULL;
        
        const char *cText = [nsText UTF8String];
        return cText ? strdup(cText) : NULL;
    }
}
