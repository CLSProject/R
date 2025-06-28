Funktionalitäten:
- Hochladen einer csv-Datei, die die Patientendaten enthält
- Auswahl aus 24 Krankheitsdatensätzen
- Einstellung von Clustering-Parametern --> auch freie Parameterwahl möglich (muss ausgewählt werden, sonst Blockade)
	- Distanzmaß auswählen, welches bei der Custerberechnung verwendet wird
	- Linkage criterion auswählen, welches angibt wie die Distanzen zwischen den Clustern berechnet werden
	- wenn Freie Parameterwahl aktiviert wird, kann Beta selbst gesetzt werden, während die anderen Werte automatisch berechnet werden. 
	- Bei Auswahl einer anderen Linkage Option werden die Parameter automatisch gesperrt
	- Cluster Kriterium auswählen, welches angibt ob nach Patienten, Genen oder beidem geclustert werden soll. 
- Visualisierungseinstellungen --> Anzahl der Cluster und Colorpattern auswählen
- Visualisierung der Ausgabe in Teil- und Vollbildmodus (aktuell nur Beispieltext, vergrößern/verkleinern durch + bzw -)
- Berechnungsstart nach betätigen des "Submit"-Buttons (aktuell noch mit 5-Sekunden Timer)
- Lade die Visualisierung neu nach betätigen des "Reload Visualization"-Buttons (aktuell noch mit 5-Sekunden Timer)

Voraussetzungen:
- install.packages("shiny")
- install.packages("shinyjs")
- install.packages("shinycssloaders")

--> Ausführen des Codes in RStudio

noch ausstehend:
- hinzufügen eines weiteren Buttons um nur die Visualisierung zu aktualisieren
