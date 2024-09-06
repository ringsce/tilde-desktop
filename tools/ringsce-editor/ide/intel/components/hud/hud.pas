{----------------------------------------------------------------------------}
{                                                                            }
{ File(s): hud.pas                                                        }
{ Content: mmo\Client - builds an intended movement command to send to the server }
{                                                                            }
{ Initial conversion by : Pedro Dias Vicente - pdvicente@gleentech.com                           }
{ Initial conversion on : 13-January-2024                                        }
{                                                                            }
{ This File contains part of convertion of MMO source to ObjectPascal.    }
{ More information about this project can be found at:                       }
{ http://www.ringscejs.gleentech.com/ekron                                            }
{                                                                            }
{ Copyright (C) 2016 - 2024 , Kreatyve Designs Inc.                                  }
{                                                                            }
{ This program is free software; you can redistribute it and/or              }
{ modify it under the terms of the GNU General Public License                }
{ as published by the Free Software Foundation; either version 2             }
{ of the License, or (at your option) any later version.                     }
{                                                                            }
{ This program is distributed in the hope that it will be useful,            }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       }
{                                                                            }
{ See the GNU General Public License for more details.                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{ * ToDo:                                                                    }
{ - All places with TODO define                                              }
{----------------------------------------------------------------------------}

{.$DEFINE TODO}
unit hud;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Interfaces;

type
  TForm1 = class(TForm)
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

