with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;

package cli is

    ---Définition du type adresse T_Adresse_IP et des constantes nécéssaires à la manipulation des adresses IP
   type T_Adresse_IP is mod 2 ** 32;  
   UN_OCTET: constant T_Adresse_IP := 2 ** 8;       -- 256
   POIDS_FORT : constant T_Adresse_IP  := 2 ** 31;

    --- Lire les commandes de l'utilisateur entrées en ligne de commande lors de l'exécution du programme
    procedure Lecture_lcommande (stats_commande : in out Integer; politique : in out integer; taille_cache : in out Integer; fichier_table : in out Unbounded_String; fichier_paquet : in out Unbounded_String; fichier_resultat : in out Unbounded_String);

    -- Convertir une règle au format "IP MASQUE INTERFACE" en trois chaînes de caractères
    procedure parse_regle(Regle: Unbounded_String ; Front_Half : in out Unbounded_String ; Middle_Half : in out Unbounded_String ; Back_Half : in out Unbounded_String);

    -- Retirer des caractères nuisibles à la lecture de la chaîne
    function Remove_char(Back_Half : in out Unbounded_String) return Unbounded_String;

    -- Convertir une IP  au format chaîne de caractères en adresse IP au format T_Adresse_IP
    function parse_ip(Regle: Unbounded_String) return T_Adresse_IP;

    -- Convertir une adresse IP au format T_Adresse_IP en chaîne de caractères
    function deparse_ip(IP1: in T_Adresse_IP) return Unbounded_String;

    --- Savoir si Masque1 < Masque2
    function  inf(Masque1 : T_Adresse_IP; Masque2 : T_Adresse_IP) return Boolean;

    ---Afin d'optimiser le cache et de maximiser la cohérence de cache, on applique une procédure
    --- à la règle issue de la table de routage pour la transformer en une règle plus générale
     
    -- Renvoyer la dernière position de 1 dans Masque en partant de la gauche du masque 
    function Pos_dernier_1(Masque : in T_Adresse_IP) return Integer;

    -- Construire un masque constitué de 1 de la gauche à Pos_dernier_1(Masque)+1 et de 0 à droite
    function construire_masque(Taille : in Integer) return T_Adresse_IP;
    
end cli;
