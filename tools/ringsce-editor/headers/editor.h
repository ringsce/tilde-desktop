#ifndef EDITOR_H
#define EDITOR_H
#include <iostream>
#include <fstream>
#include <string>
#include <gtk/gtk.h>

using namespace std;

#pragma argused

#define    MAGIC    0234

#pragma pack(push, 1)

const bool saveBAS();
const bool saveExtension();

const bool openProject();
const bool openAbout();

const bool saveBAS()
{
    ofstream myfile ("example.bas");
      if (myfile.is_open())
      {
        myfile << "This is a line.\n";
        myfile << "This is another line.\n";
        myfile.close();
      }
      else cout << "Unable to open file";
      return 0;
}


const bool openProject()
{
    ofstream myfile ("example.pjr");
      if (myfile.is_open())
      {
        myfile << "This is a line.\n";
        myfile << "This is another line.\n";
        myfile.close();
      }
      else cout << "Unable to open file";
      return 0;
}

const bool saveExtension()
{
    ofstream myfile ("example.pjr");
      if (myfile.is_open())
      {
        myfile << "This is a line.\n";
        myfile << "This is another line.\n";
        myfile.close();
      }
      else cout << "Unable to open file";
      return 0;
}

#endif // EDITOR_H
