Pour compiler, lancer la commande suivante :
ocamlbuild netlist_simulator.byte

Pour lancer sur la netlist sur le fichier exemple.net, lancer la commande :
./netlist_simulator.byte test/exemple.net

L'option -n <steps> permet de choisir le nombre de steps que fera le simulateur


Fonctionnement de rom :
L'instruction o = ROM addrs wrds addr va lire la ligne numéro addr du fichier o.data dans le dossier data
(Un fichier de test rom.net a été rajouté)







