with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions;            use Ada.Exceptions;
with SDA_Exceptions;            use SDA_Exceptions;	
with TR_liste;
with cli ; use cli;

procedure routeurll is
   ----Variables, fonctions et types de la Part 1
   ----------------------------------------------
	--Déclaration des variables et initialisation avec les valeurs par dÃ©faut

	stats_commande : Integer := 1;                                               --valeur par défaut de la commande statistiques
	fichier_paquet : Unbounded_String := To_Unbounded_String("paquets.txt");    --valeur par défaut du fichier des paquets
	fichier_table : Unbounded_String := To_Unbounded_String("table.txt");     --valeur par défaut du fichier de la table de routage
	fichier_resultat : Unbounded_String := To_Unbounded_String("resultats.txt"); --valeur par défaut du fichier de sortie
	taille_cache : Integer := 10;                                                       --variable définissant la taille_cache du cache
	politique : Integer := 0;                                                    --variable définissant la politique du cache, valeur par défaut 0
   nb_defaut_cache : Integer := 0;
   nb_demande_route : Integer := 0;
   taux_defaut_cache : Float := 0.0;

   ----Variables, fonctions et types de la Part 2
   ----------------------------------------------

   Str : Unbounded_String; 
   IP : Unbounded_String;
   Front_Half :  Unbounded_String;
   Middle_Half : Unbounded_String;
   Back_Half  : Unbounded_String;
   IPvf : T_Adresse_IP;
   Masquevf : T_Adresse_IP;
   
   Input : File_Type;
   Fichier_TR : Unbounded_String ;

   function "+" (Item : in String) return Unbounded_String
        renames To_Unbounded_String;
   function "-" (Item : in Unbounded_String) return String
        renames To_String;

----Variables, fonctions et types de la Part 3
   ----------------------------------------------
    ---Importation du module TR_liste
    package TRL is
		new TR_liste (T_IP => T_Adresse_IP, T_Masque => T_Adresse_IP, T_Interface => Unbounded_String);  
    use TRL;

    TR : T_liste;
    cache : T_liste;

    --taille_masque : Integer;

	Nom_Fichier_Resultat : Unbounded_String;
   Nom_Fichier_Paquet : Unbounded_String;


      
   procedure Afficher (IPtest : T_Adresse_IP ; Masque : T_Adresse_IP; Inter : Unbounded_String) is      --afficher les routes sur demande
      Texte : Unbounded_String;
      IPtemp : Unbounded_String;
      Masquetemp : Unbounded_String;
   begin
      IPtemp := deparse_ip(IPtest);
      Masquetemp :=deparse_ip(Masque);
      Texte := IPtemp & " | " & Masquetemp & " | " & Inter;
      Put_Line(Texte);
   end Afficher;

            
   function comparer (IPtest : T_Adresse_IP; Masque : T_Adresse_IP; IPdest : T_Adresse_IP) return Boolean is    --type entrees  et format
      begin
         if ((IPtest and Masque) = IPdest) then 
            return True;
         else 
            return False;
         end if;
      end comparer;

   procedure AfficherListe is
     new Pour_Chaque_liste (Afficher);

   function TraitIPListe is
      new TraiterIP_liste (comparer,inf);

   function PresCache is
      new Cle_Match_liste (comparer);

   function AjCache is
      new AjouterCache (comparer, inf,Pos_dernier_1);

         procedure Execution_commandes is 

         Adresse_IP : Unbounded_String; 
         Texte : Unbounded_String ;             
         Numero_Ligne : Integer;    
         Type_Fichier_Paquet : File_Type;	-- Le descripteur du ficher d'entrÃ©e
         Type_Fichier_Resultat : File_Type;	-- Le descripteur du ficher de fichier_resultat
         DestTemp : T_Adresse_IP;  -- Variable définissant l'adresse IP de la route
         Interf : Unbounded_String ;  -- Variable string définissant une interface
         IPdest : Unbounded_String;  -- Variable string définissant l'adresse IP d'une destination
         Texte_temp : Unbounded_String;   -- Variable string temporaire
         IP_actuelle : T_Adresse_IP;  -- Variable définissant l'adresse IP de la route
         minc : T_Adresse_IP;     -- Variable définissant la destination de la règle dont le compteur est minimal
         maxc : T_Adresse_IP;     -- Variable définissant la destination de la règle dont le compteur est maximal


      begin  
            Lecture_lcommande (stats_commande,politique,taille_cache,fichier_table,fichier_paquet,fichier_resultat);

            ----Initialisation de la liste LCA servant pour les politiques FIFO, LRU et LFU
            Initialiser_liste(cache);

            ----Ouverture des fichiers contenant les paquets et de sortie qui contiendra les bonnes interfaces
            Nom_Fichier_Paquet := fichier_paquet;
            Nom_Fichier_Resultat := fichier_resultat;
            Create (Type_Fichier_Resultat, Out_File, To_String (Nom_Fichier_Resultat));
            Open (Type_Fichier_Paquet, In_File, To_String (Nom_Fichier_Paquet));
            
            begin
               
            loop
            Numero_Ligne := Integer (Line (Type_Fichier_Paquet));     --On récupère le numéro de la ligne de commande dans le fichier paquets
            Adresse_IP := Get_Line (Type_Fichier_Paquet);             
            Adresse_IP := Trim (Adresse_IP, Both);
            
            -- Commande table
            if Adresse_IP = To_Unbounded_String("table") then 
                  Put_line("------------------------------------------------");
                  Put_line("table (ligne" & integer'image(Numero_Ligne) & ")");
                  Put_line("------------------------------------------------");
                  AfficherListe(TR); 
                  Put_line("------------------------------------------------");                  
            -- Commande fin
            elsif Adresse_IP = To_Unbounded_String("fin") then
                     Put_line("------------------------------------------------");
                     Put_line("fin (ligne" & integer'image(Numero_Ligne) & ")");
                     Put_line("------------------------------------------------");
                     exit;            
            --Commande cache
            elsif Adresse_IP = To_Unbounded_String("cache") then
                     Put_line("------------------------------------------------");
                     Put_line("cache (ligne" & integer'image(Numero_Ligne) & ")");
                     Put_line("------------------------------------------------");
                     AfficherListe(cache);
                     Put_line("------------------------------------------------");   

	   elsif  Adresse_IP = To_Unbounded_String("range a..z") or Adresse_IP = To_Unbounded_String("range A..Z") then
	   
	   	raise Constraint_Error;

	  
	   else
            -- Traitement des routes

                     --Incrémentation du compteur de nombre de routes
                     nb_demande_route := nb_demande_route + 1;

                     Adresse_IP := Trim (Adresse_IP, Right);
                     --Traitement de l'adresse IP
                     IP_actuelle := parse_IP(Adresse_IP);

                     ---L'utilisateur a demandé un cache
                     if taille_cache /= 0 then
                     
                        --1er cas : l'IP a une règle correspondante dans le cache 
                        if  PresCache(cache, IP_actuelle) then  
                           case politique is 
                              when 0 => --cache FIFO : ne rien faire
                                 DestTemp := TraitIPListe(cache,IP_actuelle);
                              when 1 => --cache LRU
                                 DestTemp := TraitIPListe(cache,IP_actuelle);                            
                                 Incrementer_rec(cache);
                              when 2 => --cache LFU
                                 DestTemp := TraitIPListe(cache,IP_actuelle);
                                 Incrementer_freq(cache,IP_actuelle);
                              when others => Null;
                           end case;
                           Interf := La_Donnee_Inter_liste(cache,DestTemp);
                           IPdest := Adresse_IP;
                           Texte_temp := IPdest & " | " & Interf;
                           --On écrit l'IP destination et l'interface utilisée dans le fichier résultat
                           Put(Type_Fichier_Resultat, Texte_temp); 
                           New_Line (Type_Fichier_Resultat);

                        --2eme cas : l'IP n'a pas de règle correspondante dans le cache
                        else 

                           ----Recherche de la règle correspondante directement dans la table de routage
                           DestTemp := TraitIPListe(TR,IP_actuelle);

                           ----Définition de la règle correspondante
                           Interf := La_Donnee_Inter_liste(TR,DestTemp);
                           Masquevf := La_Donnee_Masque_liste(TR, DestTemp);
                           IPdest := Adresse_IP;
                           
                           ----Ecriture dans le fichier des résultats
                           Texte_temp := IPdest & " | " & Interf;
                           Put(Type_Fichier_Resultat, Texte_temp);  
                           New_Line (Type_Fichier_Resultat); 

                           IP_actuelle := IP_actuelle and Masquevf;
                           ----Construction de la règle modifiée
                           --if Taille_liste(cache) = 0 then
                              --taille_masque := Pos_dernier_1(IP_actuelle);
                           --else
                             -- taille_masque := AjCache(cache,IP_actuelle); ---emplacement du dernier 1 dans l'avant dernier masque le plus long
                           --end if;
                           --Masquevf := construire_masque(taille_masque);
                           --IP_actuelle := IP_actuelle and Masquevf;

                           ------Vérifions que le cache ne soit pas plein
                           if Taille_liste(cache) < taille_cache  and deparse_ip(IP_actuelle) /= "0.0.0.0" then
          	
                             		 Ajouter_regle_liste(cache, IP_actuelle , Interf , Masquevf);
                           	
                           ---Si le cache est plein, alors il faut regarder la politique considérée
                           elsif Taille_liste(cache) >= taille_cache + 1  and deparse_ip(IP_actuelle) /= "0.0.0.0" then
                                 nb_defaut_cache := nb_defaut_cache + 1;

                                 case politique is

                                 ---politique FIFO
                                  when 0 => 
                                    Supprimer_fin(cache); 
                                
                                    Ajouter_regle_liste(cache,IP_actuelle , Interf , Masquevf);
				    
                                 ----politique LRU
                                  when 1 => 
                                    --Supprimer la donnée la moins récemment utilisée du cache et de la liste
                                    minc := Min(cache); 
                                    Supprimer_par_destination_liste(cache,minc);
                                    --Ajouter la nouvelle règle au cache et à la liste
                                  
          			   
                                    Ajouter_regle_liste(cache,IP_actuelle , Interf , Masquevf);
                                   
                                    Incrementer_rec(cache);

                                 ---politique LFU
                                  when 2 => 
                                    --Supprimer la donnée la moins fréquemment utilisée du cache et de la liste
                                    maxc := Max(cache);
                                    Supprimer_par_destination_liste(cache,maxc);

                                    --Ajouter la nouvelle règle au cache et à la liste
                                   
                                    Ajouter_regle_liste(cache,IP_actuelle , Interf , Masquevf);
			            
                                    Incrementer_freq(cache, IP_actuelle);

                                 when others => Null;
                                 end case;
                           else 
                             	null;
                           end if;
                           end if;

                     ---L'utilisateur n'a pas demandé de cache
                     else 
                        DestTemp := TraitIPListe(TR,parse_ip(Adresse_IP)); 	
                        Interf := La_Donnee_Inter_liste(TR,DestTemp);
                        IPdest := Adresse_IP;
                        Texte_temp := IPdest & ' ' & Interf;
                        Put(Type_Fichier_Resultat, Texte_temp);                  --On affiche l'IP destination et l'interface utilisée dans le fichier résultat
                        New_Line (Type_Fichier_Resultat);
                     end if;
        
            end if;
                  exit when End_Of_File (Type_Fichier_Paquet);
               end loop;
               
            exception
            when Constraint_Error => Put_Line("Saisir des instructions correctes dans le fichier paquets.txt"); 
            stats_commande := 0;
            --Put("Cle absente");
            -- New_Line;
               when End_Error =>
                  Put ("Blancs en surplus Ã  la fin du fichier.");
            end;
            Close (Type_Fichier_Paquet);
            Close (Type_Fichier_Resultat);
      exception
            when Regle_non_trouvee => Null;
         when E : others =>
            Put_Line (Exception_Message (E));
      end Execution_commandes;
   
   procedure Afficher_nb_defaut_cache(nb_defaut_cache : Integer) is
   begin
      Put("Nombre de défauts du cache : " & nb_defaut_cache'Image);
      New_Line;
   end Afficher_nb_defaut_cache;

   procedure Afficher_nb_demande_route(nb_demande_route : Integer) is
      begin
      Put("Nombre de demandes de route : " & nb_demande_route'Image);
      New_Line;
    end Afficher_nb_demande_route;

    procedure Afficher_taux_defaut_cache(taux_defaut_cache : Float) is
    begin
      Put("Taux de défauts du cache : "&taux_defaut_cache'Image);
      New_Line;
    end Afficher_taux_defaut_cache;



-----début du programme
begin

   --Part 2 : Initialisation de la table de routage et remplissage à l'aide du fichier
   Initialiser_liste(TR);
   begin
      Open (File => Input,
            Mode => In_File,
            Name => -fichier_table);
   end;
   loop
      declare
        Str : Unbounded_String;
      begin
         Str := Get_Line (Input);
         parse_regle(Str,Front_Half,Middle_Half,Back_Half);
         IPvf:=parse_IP(Front_Half);
         Masquevf:=parse_IP(Middle_Half);
         Enregistrer_liste(Sda      => TR,
                     IP        => IPvf,
                     Interfac => Back_Half,
                     Masque   => Masquevf);
      end;
      exit when End_Of_File (Input);
   end loop;     
   Close (Input);

--Traiter les lignes du fichier paquets.txt

   Nom_Fichier_Paquet:=fichier_paquet;
   Nom_Fichier_Resultat:= fichier_resultat;
   Execution_commandes;

--Part 4 : affichage post exécution
if stats_commande = 1 then
       Put_Line("Statistiques du routeur : ");
       Afficher_nb_demande_route(nb_demande_route);
       if taille_cache /= 0 then
        taux_defaut_cache := float(nb_defaut_cache) / float(nb_demande_route);
        Afficher_nb_defaut_cache(nb_defaut_cache);
        Afficher_taux_defaut_cache(taux_defaut_cache);
      end if;
else
    Null;
end if;

end routeurll;
