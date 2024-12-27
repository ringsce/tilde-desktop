#include <exec/types.h>
#include <dos/dos.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/zip.h> // Ensure zip.library is available

#define ZIP_PATH "ram:archive.zip"
#define DIR_PATH "ram:newdir"
#define MOVED_DIR "newdir"

// Function to compress a directory into a ZIP file
BOOL CompressDirToZip(const char *dirPath, const char *zipPath) {
    struct Library *ZipBase = OpenLibrary("zip.library", 0);
    if (!ZipBase) {
        Printf("Failed to open zip.library\n");
        return FALSE;
    }

    struct ZipHandle *zip = CreateZip(zipPath, NULL);
    if (!zip) {
        Printf("Failed to create ZIP archive: %s\n", zipPath);
        CloseLibrary(ZipBase);
        return FALSE;
    }

    BOOL result = AddZipDir(zip, dirPath, MOVED_DIR);
    if (!result) {
        Printf("Failed to add directory to ZIP archive\n");
    } else {
        Printf("Successfully added directory to ZIP archive\n");
    }

    CloseZip(zip);
    CloseLibrary(ZipBase);

    return result;
}

int main() {
    // Create a directory
    BPTR dirLock = CreateDir(DIR_PATH);
    if (!dirLock) {
        Printf("Failed to create directory: %s\n", DIR_PATH);
        return RETURN_FAIL;
    }
    UnLock(dirLock); // Directory created, unlock it

    Printf("Directory created successfully: %s\n", DIR_PATH);

    // Compress the directory to a ZIP file
    if (!CompressDirToZip(DIR_PATH, ZIP_PATH)) {
        Printf("Failed to compress directory to ZIP\n");
        return RETURN_FAIL;
    }

    Printf("Directory successfully compressed to ZIP: %s\n", ZIP_PATH);

    return RETURN_OK;
}
