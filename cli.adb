with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with SDA_Exceptions;            use SDA_Exceptions;

package body cli is


    procedure Lecture_lcommande (stats_commande : in out Integer; politique : in out integer; taille_cache : in out Integer; fichier_table : in out Unbounded_String; fichier_paquet : in out Unbounded_String; fichier_resultat : in out Unbounded_String) is
      skipp : Boolean := False;
    begin 
        for Arg in 1..Argument_Count loop
            if skipp then
                skipp := False;
            else
               if Argument(Arg) = "-s" then
                  stats_commande := 1;               --Afficher statistiques
               elsif Argument(Arg)= "-S" then
                  stats_commande := 0;               --Ne pas afficher statistique
               elsif Argument(Arg) = "-p" then
                  skipp := True;
                  if Argument(Arg+1) = "FIFO" then
                     politique := 0;               --politique FIFO
                     elsif Argument(Arg+1) = "LRU" then
                           politique := 1;               --politique LRU
                     elsif Argument(Arg+1) = "LFU" then
                              politique := 2;               --politique LFU
                     else 
                        raise Mauvaise_commandeP;
                     end if;
               elsif Argument(Arg) = "-c" then
                     skipp := True;
                     taille_cache := Integer'value(Argument(Arg+1));  --taille_cache du cache
               elsif Argument(Arg) = "-t" then
                     skipp := True;
                     fichier_table := To_Unbounded_String(Argument(Arg+1));      
               elsif Argument(Arg) = "-P" then
                     skipp := True;
                     fichier_paquet := To_Unbounded_String(Argument(Arg+1));     
               elsif Argument(Arg) = "-r" then
                     skipp := True;
                     fichier_resultat := To_Unbounded_String(Argument(Arg+1));   
               else
                     raise Constraint_Error;        
               end if;
            end if;
        end loop;
    exception 
	    when Mauvaise_commandeP => Put_Line("Ecrire une politique correcte");
	    when Constraint_Error =>  Put_Line("Ecrire une commande correcte (voir manuel utilisateur)");
   end Lecture_lcommande;

   function "+" (Item : in String) return Unbounded_String
        renames To_Unbounded_String;
   function "-" (Item : in Unbounded_String) return String
        renames To_String;


   -----Procedure permettant de séparer les 3 parties d'une règle (l'adresse IP, le masque et l'interface)
   procedure parse_regle(Regle: Unbounded_String ; Front_Half : in out Unbounded_String ; Middle_Half : in out Unbounded_String ; Back_Half : in out Unbounded_String) is

      compteur_premiere : Integer;       --Variable prenant la valeur de l'indice du dernier élément de l'adresse IP pour une ligne du paquet
      compteur_seconde : Integer;        --Variable prenant la valeur de l'indice du dernier élément du masque pour une ligne du paquet
      compteur : Integer;
   begin
     compteur_premiere := 0;
      compteur_seconde := 0;
      compteur := 0;
      
   for i of To_String(Regle) loop
      if i= ' ' and compteur_premiere = 0 then
         compteur_premiere := compteur;               
      elsif i= ' ' and compteur_premiere /= 0 then
         compteur_seconde := compteur;                
      end if;
      compteur := compteur +1;
   end loop;
      Front_Half:= +To_String(Regle)(To_String(Regle)'first..compteur_premiere); -- On récupère l'adresse IP
      Middle_Half := +To_String(Regle)(compteur_premiere+2..compteur_seconde);   -- On récupère le masque associé à l'adresse IP
      Back_Half := +To_String(Regle)(compteur_seconde+2..To_String(Regle)'last); -- On récupère l'interface associé à l'adresse IP et au masque
   end parse_regle;

   function Remove_char(Back_Half : in out Unbounded_String) return Unbounded_String is
   begin
      if To_String(Back_Half)'Length > 1 then
            Back_Half := +To_String(Back_Half)(To_String(Back_Half)'first..To_String(Back_Half)'last-1);
      end if;
      return Back_Half;
   end Remove_char;

   -----Fonction permettant de transformer une IP de string à T_Adresse_IP
   function parse_ip(Regle: Unbounded_String) return T_Adresse_IP is
      compteur_premiere : Integer;                        --Variable prenant la valeur de l'indice du dernier élément du premier octet
      compteur_seconde : Integer;                         --Variable prenant la valeur de l'indice du dernier élément du deuxième octet
      compteur_trois : Integer;                           --Variable prenant la valeur de l'indice du dernier élément du troisième octet
      compteur : Integer;
      Front_Half :  Unbounded_String;
      Middle_Half : Unbounded_String;
      Middle_M_Half : Unbounded_String;
      Back_Half  : Unbounded_String;
      q1 : Integer ;
      IP : T_Adresse_IP;

   begin
   compteur_premiere := 0;
   compteur_seconde := 0;
   compteur_trois :=0;
   compteur := 0;
   for i of To_String(Regle) loop
      if i= '.' and compteur_premiere = 0 then
         compteur_premiere := compteur;                                     -- On récupère l'indice du dernier élément du premier octet de l'adresse IP
      elsif i= '.' and compteur_premiere /= 0 and compteur_seconde=0 then
            compteur_seconde := compteur;                                   -- On récupère l'indice du dernier élément du deuxième octet de l'adresse IP
      elsif i= '.' and compteur_premiere /= 0 and compteur_seconde /=0 then
            compteur_trois := compteur;                                     -- On récupère l'indice du dernier élément du troisième octet de l'adresse IP
      end if;
      compteur := compteur +1;
   end loop;
      Front_Half:= +To_String(Regle)(To_String(Regle)'first..compteur_premiere); --On récupère le premier octet sous format string
      Middle_Half := +To_String(Regle)(compteur_premiere+2..compteur_seconde);   --On récupère le deuxième octet sous format string
      Middle_M_Half := +To_String(Regle)(compteur_seconde+2..compteur_trois);    --On récupère le troisième octet sous format string
      Back_Half := +To_String(Regle)(compteur_trois+2..To_String(Regle)'last);   --On récupère le dernier octet sous format string
      q1 := Integer'Value (To_String (Front_Half)) ;                             --On convertit le string en une valeur Integer
      IP := T_Adresse_IP(q1);
      q1 := Integer'Value (To_String (Middle_Half));
      IP := IP * UN_OCTET + T_Adresse_IP(q1);                                    -- On concatène les octets
      q1 := Integer'Value (To_String (Middle_M_Half));
      IP := IP * UN_OCTET + T_Adresse_IP(q1);
      Back_Half := Remove_char(Back_Half);
      q1 := Integer'Value (To_String ( Back_Half));
      IP := IP * UN_OCTET + T_Adresse_IP(q1);
      
      return IP;
   end parse_IP;

    -----Fonction permettant de transformer une T_Adresse_IP en IP de string 
   function deparse_ip(IP1: in T_Adresse_IP) return Unbounded_String is
            IP2 : T_Adresse_IP;
            UN_OCTET: constant T_Adresse_IP := 2 ** 8;
            IP3:  Unbounded_String;     
            Q1 : Unbounded_String;      --Variable string prenant la valeur du dernier octets de IP1
            Q2 : Unbounded_String;      --Variable string prenant la valeur du troisième octets de IP1
            Q3 : Unbounded_String;      --Variable string prenant la valeur du deuxième octet de IP1
            Q4 : Unbounded_String;      --Variable string prenant la valeur du premier octet de IP1
            
	begin
               IP2 := IP1;
               -- Conversion d'un T_Adresse_IP en String
	            Q1 := +Integer'Image(Natural (IP2 mod UN_OCTET));     --On convertit l'Integer en String
               IP2 := IP2 / UN_OCTET;
               Q2 := +Integer'Image(Natural (IP2 mod UN_OCTET));
	            IP2 := IP2 / UN_OCTET;
               Q3 := +Integer'Image(Natural (IP2 mod UN_OCTET));
	            IP2 := IP2 / UN_OCTET;
               Q4 :=  +Integer'Image(Natural (IP2 mod UN_OCTET));
               IP3 := Q4&'.'&Q3&'.'&Q2&'.'&Q1;               -- On concatène les quatre strings pour former l'adresse IP
               return IP3;
   end deparse_ip;

   function inf (Masque1 : T_Adresse_IP; Masque2 : T_Adresse_IP) return Boolean is 
   begin 
        if Masque1 < Masque2 then
                return True;
        else
                return False;
        end if;
   end inf;

   function Pos_dernier_1(Masque : in T_Adresse_IP) return Integer is
   compteur: Integer:= 0;
   Miroir : T_Adresse_IP := Masque;
   begin
      while (Masque*2**(32-compteur) and POIDS_FORT) = 0 loop
         compteur := compteur + 1;
      end loop;
   return compteur;
   end Pos_dernier_1;

   function construire_masque(Taille : in Integer) return T_Adresse_IP is
   Masque : T_Adresse_IP := 0;
   begin
   for  I in 1..32 loop
      if I < Taille +3 then
         Null;
      else
         Masque := Masque + 2**(I);
      end if;
   end loop;
   return Masque;
   end construire_masque;

end cli;
