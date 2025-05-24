# COBOL-Bulletin
## IDENTIFICATION DIVISION.
```cobol
       PROGRAM-ID. report.
       AUTHOR. AlexEnCode&Terry.
```
## ENVIRONMENT DIVISION.
### CONFIGURATION SECTION.
```cobol
      *Permet la lecture des formats européens.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
```
### INPUT-OUTPUT SECTION.
### FILE-CONTROL.
```cobol
      *Lecture séquentielle par ligne.
           SELECT F-INPUT
           SELECT F-OUTPUT

```
## DATA DIVISION.
### FILE SECTION.
```cobol
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
      *Définition des structures pour la gestion fichier.
       01  REC-F-INPUT-2  *> Gestion de la clé pour choix structure.
       01  REC-STUDENT.   *> Structure utilisé pour les étudiants.
       01  REC-COURSE.    *> Structure utilisée pour les cours.

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
       01  REC-F-OUTPUT        *> Ligne de sortie
```
### WORKING-STORAGE SECTION.
```cobol
      *Flag pour gestion du fichier d'ouverture.
       01  F-INPUT-STATUS
      *Définition d'un groupe de variable comprenant un tableau
      *de taille dynamique pour les étudiants.
       01  DATA-STUDENT.
      *Définition d'un groupe de variable comprenant un tableau
      *de taille dynamique pour les cours.
       01  DATA-COURSE.
      *Définition d'un groupe de variable comprenant un tableau
      *de taille dynamique pour stocker les moyennes de la classe.
       01  WS-AVERAGE.
      *Définition d'un groupe de variable indépendantes
      * pour les calculs de moyenne.
       01  WS-CALCUL.
      *Définition de plusieurs groupes de variable pour sortie écriture.
       01 WS-HEADER.
       01 WS-BODY.
       01 WS-TOTAL.
       01 WS-COURSE-INFO.
       01 WS-STATS.
       01 WS-FOOTER.
```
## PROCEDURE DIVISION.
```cobol
      *Appel de tout les paragraphes pour
      *visualisation rapide de toutes les fonctions du Programme
           STOP RUN.
```
#### 0100-READ-START.
```cobol
      *Paragraphe gérant la lecture du fichier importé
      *Et le stockage de ses informations dans la WS.
```
#### 0200-DISPLAY-START.
```cobol
      *Paragraphe gérant l'affichage console des données stockées.
```
#### 0300-AVERAGE-START.
```cobol
      *Paragraphe gérant le calcul de toutes les moyennes.
```
#### 0400-LASTNAME-DUPLICATES-START.
```cobol
      *Paragraphe pour gérer les doublons dans les noms.
```
#### 0500-SORT-NAME-START.
```cobol
      *Paragraphe pour gérer le tri du tableau STUDENT par la key LASTNAME.
```
#### 0600-WRITE-START.
```cobol
      *Paragraphe gérant le remplissage ds structures d'éditions
      *ainsi que l'écriture du fichier de sortie dans le format voulu.
```
# AXE D'AMELIORATION.
```
- Ajouter du contrôle d'entrée (ex: caractères spéciaux)
- Traiter les doublons de matière
- Traiter les notes absentes
- Ajout d'appréciations en fonction des notes ou moyennes
```
