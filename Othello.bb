;   Das Programm darf frei verwendet werden
;   Programmautor: Darko Najman, 04.08.2005
;   Othello Version 1.1
;
Graphics 320,320,16,2
SetBuffer BackBuffer()
ClsColor 120,120,120
AppTitle "Othello"

;Variablendeklarationen
Global x%=0   ;Schleifenvariable X-Spielfeld
Global y%=0   ;Schleifenvariable Y-Spielfeld
Global q%=0   ;Allgemeine Schleifenvariable
Global w%=0   ;Allgemeine Schleifenvariable
Global ok%=0   ;Fehlervariable zum finden
Global mx%=0   ;Maus-X-Spielfeldposition
Global my%=0   ;Maus-Y-Spielfeldposition
Global ic%=1   ;Spielmodus bestimmen
Global gn%=0   ;Geht noch ein weiterer Zug
Global s1%=0   ;Spielwertzähler Spieler
Global s2%=0   ;Spielwertzähler Computer
Global cx%=0   ;Computer X-Position
Global cy%=0   ;Computer Y-Position
Global ct%=0   ;Computer Treffpunkte

;Spielfeld
Dim xy%(9,9)
xy(4,4)=1
xy(5,4)=2
xy(4,5)=2
xy(5,5)=1

;Koordination X
Dim xc%(7)
xc(0)=0
xc(1)=1
xc(2)=1
xc(3)=1
xc(4)=0
xc(5)=-1
xc(6)=-1
xc(7)=-1

;Koordination Y
Dim yc%(7)
yc(0)=-1
yc(1)=-1
yc(2)=0
yc(3)=1
yc(4)=1
yc(5)=1
yc(6)=0
yc(7)=-1

;Vor/Nachteile
Dim vn%(9,9)

;Sprungmarke setzen
Restore vornachteile
;Vor/Nachteiletabelle einlesen
For y=0 To 9:Read Level,vn(0,y),vn(1,y),vn(2,y),vn(3,y),vn(4,y),vn(5,y),vn(6,y),vn(7,y),vn(8,y),vn(9,y):Next

;Zufallsgenerator
SeedRnd MilliSecs()
;Level Zeichnen
zeichnen()
;Beginerentscheid
ic=Rand(1,2)
finden(ic)
Delay 500




;HAUPTSCHLEIFE
While Not KeyHit(1)
 ;Computerspielmodus
 If ic=2 Then
  ;Computer spielen
  computer()
  ;Level Zeichnen
  zeichnen()
  ;Für Benutzermodus
  finden(1):ic=1
 End If
 ;Benutzerspielmodus
 If ic=1 Then
  ;Maus erfassen
  mx=MouseX()/32
  my=MouseY()/32
  ;Maus einschränken
  If mx>9 Then mx=9
  If my>9 Then my=9
  ;Wenn Auswahl mit Maus getroffen
  If MouseHit(1) And xy(mx,my)<0 Then ic=2:steine(mx,my,1)
  ;Level Zeichnen
  zeichnen()
 End If
Wend
;Wer gewonnen
finden(1)
;Spiel beenden
beenden(0)




;COMPUTER-KI
Function computer()
 cx=0
 cy=0
 ct=0
 ;Vermenschlichung
 Delay 500
 ;Möglichkeit finden
 finden(2)
 ;Möglichkeiten spielen
 For x=0 To 9
  For y=0 To 9
   If xy(x,y)<0 Then
    ;Vorteile sichern
    xy(x,y)=xy(x,y)-vn(x,y)
    ;Spielentscheidung festlegen
    If ct>xy(x,y) Then cx=x:cy=y:ct=xy(x,y)
    If ct=xy(x,y) Then If Rnd(0,1)>0.5 Then cx=x:cy=y:ct=xy(x,y)
   End If
  Next
 Next
 ;Steinreihen setzen
 steine(cx,cy,2)
End Function




;ZEICHNEN
Function zeichnen()
 Cls
 ;Spielsteine zeichnen
 For x=0 To 9
  For y=0 To 9
   Color 080,080,080:Rect x*32,y*32,32,32,0
   If xy(x,y)=1 Then Color 240,240,240:Oval 2+x*32,2+y*32,28,28,1
   If xy(x,y)=2 Then Color 000,000,000:Oval 2+x*32,2+y*32,28,28,1
   If xy(x,y)<0 Then Color 140,140,140:Rect 6+x*32,6+y*32,20,20,0;:Color 0,0,0:Text 2+x*32,2+y*32,Abs(xy(x,y))
   If ic=1 Then Color 240,240,240:Rect mx*32,my*32,32,32,0
  Next
 Next
 ;Anzahl Steine anzeigen
 If MouseDown(2) Then
  Color 080,080,080:Rect 137,24,45,15,1
  Color 255,255,255:Rect 136,23,47,17,0
  Text 139,25,Right$("00"+s1,2)+"/"+Right$("00"+s2,2)
 End If
 Flip
End Function




;MÖGLICHKEITEN
Function finden(s)
 ;Für Spielende bereiten
 gn=0:s1=0:s2=0
 ;Spielfeld überprüfen
 For x=0 To 9
  For y=0 To 9
   ;Alte Möglichkeit löschen
   If xy(x,y)<0 Then xy(x,y)=0
   ;Spielsteine zusammenzählen
   If xy(x,y)=1 Then s1=s1+1
   If xy(x,y)=2 Then s2=s2+1
   ;Eventuelle Möglichkeit prüfen
   If xy(x,y)=0 Then
    ;Alle 8 Richtungen prüfen
    For q=0 To 7
     ;Richtung bis Spielfeldende
     ok=0:For w=1 To 9
      ;Nur wenn kein Spielfeldende
      If Not x+xc(q)*w<0 Then
       If Not x+xc(q)*w>9 Then
        If Not y+yc(q)*w<0 Then
         If Not y+yc(q)*w>9 Then
          ;Spielsteinmöglichkeit prüfen
          If xy(x+xc(q)*w,y+yc(q)*w)=0 Then ok=-1
          If xy(x+xc(q)*w,y+yc(q)*w)<0 Then ok=-1
          If xy(x+xc(q)*w,y+yc(q)*w)=3-s And ok<>-1 Then ok=1
          If xy(x+xc(q)*w,y+yc(q)*w)=s And ok=1 Then gn=1:xy(x,y)=xy(x,y)-w+1
          If xy(x+xc(q)*w,y+yc(q)*w)=s Then ok=-1
         End If
        End If
       End If
      End If
     Next
    Next
   End If
  Next
 Next
 ;Keine Möglichkeiten
 If gn=0 Then beenden(s)
End Function




;SPIELSTEINE ZEICHNEN
Function steine(x,y,s)
 ;Ersten Stein setzen
 xy(x,y)=s
 ;Alle 8 Richtungen prüfen
 For q=0 To 7
  ;Richtung bis Spielfeldende
  ok=0:For w=1 To 9
   ;Nur wenn kein Spielfeldende
   If Not x+xc(q)*w<0 Then
    If Not x+xc(q)*w>9 Then
     If Not y+yc(q)*w<0 Then
      If Not y+yc(q)*w>9 Then
       ;Spielsteinmöglichkeit prüfen
       If xy(x+xc(q)*w,y+yc(q)*w)=0 Then ok=-1
       If xy(x+xc(q)*w,y+yc(q)*w)<0 Then ok=-1
       If xy(x+xc(q)*w,y+yc(q)*w)=s And ok=0 Then ok=1
      End If
     End If
    End If
   End If
   ;Nur bis Reichweite der Richtung
   If ok=1 Then
    For w=1 To 9
     ;Nur wenn kein Spielfeldende
     If Not x+xc(q)*w<0 Then
      If Not x+xc(q)*w>9 Then
       If Not y+yc(q)*w<0 Then
        If Not y+yc(q)*w>9 Then
         If xy(x+xc(q)*w,y+yc(q)*w)=s Then ok=-1
         If ok=1 Then xy(x+xc(q)*w,y+yc(q)*w)=s
        End If
       End If
      End If
     End If
    Next
  End If
  Next
 Next
End Function




;SPIEL BEENDEN
Function beenden(s)
 ;Aktuelles Bild zeigen
 Flip:Delay 1000
 ;Bild verdunkeln
 Color 0,0,0
 For q=-320 To 320 Step 2
  Line q,0,q+320,320
 Next
 ;Texthintergrund
 Color 40,40,40
 Rect 0,25,320,15,1
 Rect 0,57,320,15,1
 Rect 0,89,320,15,1
 Rect 0,121,320,15,1
 Rect 0,217,320,15,1
 Rect 0,249,320,15,1
 Rect 0,281,320,15,1
 ;Informationsausgabe
 Color 255,255,255
 If s=0 Then Text 0,25,"Spiel abgebrochen, Spiel verloren"
 If s=1 Then Text 0,25,"Sie haben keine Möglichkeiten mehr"
 If s=2 Then Text 0,25,"Computer hat keine Möglichkeiten mehr"
 Text 0,57,s1+" Spielsteine Spieler"
 Text 0,89,s2+" Spielsteine Computer"
 If s1<s2 Then s1=0:s2=1:Text 0,121,"Sie haben verloren"
 If s>0 And s1>s2 Then s1=1:s2=0:Text 0,121,"Sie haben gewonnen"
 If s=0 And s1=>s2 Then s1=0:s2=1:Text 0,121,"Warum tun Sie sowas?"
 If s1=s2 Then s1=0:s2=0:Text 0,121,"Das Spiel ist unentschieden"
 ;Prüfen ob Spielstände verfügbar
 If FileType("othello.dat")=1 Then
  ;Bisherige Spielstände lesen
  othello=ReadFile("othello.dat")
  s1=s1+ReadInt(othello)
  s2=s2+ReadInt(othello)
  CloseFile othello
 End If
 ;Bisherige Ergebnisse anzeigen
 Text 0,217,s1+" Spiele gewonnen"
 Text 0,249,s2+" Spiele verloren"
 If (s1+s2)>0 Then Text 0,281,100/(s1+s2)*s1+"% aller Spiele gewonnen"
 ;Neue Werte in Datei schreiben
 othello=WriteFile("othello.dat")
 WriteInt othello,s1
 WriteInt othello,s2
 CloseFile othello
 ;Bild zeigen
 Flip
 ;Warten auf Reaktion
 FlushKeys
 WaitKey
 End
End Function




.vornachteile
Data 0, 8,1,6,3,4,4,3,6,1,8
Data 1, 1,0,1,1,1,1,1,1,0,1
Data 2, 6,1,4,2,2,2,2,4,1,6
Data 3, 3,1,2,1,2,2,1,2,1,3
Data 4, 4,1,2,2,2,2,2,2,1,4
Data 5, 4,1,2,2,2,2,2,2,1,4
Data 6, 3,1,2,1,2,2,1,2,1,3
Data 7, 6,1,4,2,2,2,2,4,1,6
Data 8, 1,0,1,1,1,1,1,1,0,1
Data 9, 8,1,6,3,4,4,3,6,1,8