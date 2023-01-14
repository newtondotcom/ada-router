with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with SDA_Exceptions; 		use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
	--! Les Unbounded_String ont une capacité variable, contrairement au String
	--! pour lesquelles une capacité doit être fixée.
with cli;  use cli;
with TR_liste;


procedure test_tr_liste is
    
    package TR_package is
		new TR_liste (T_IP => T_Adresse_IP, T_Masque => T_Adresse_IP , T_Interface => Unbounded_String);
	use TR_package;


-- Retourner une chaîne avec des guillemets autour de S
	function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;

	-- Utiliser & entre String à gauche et Unbounded_String à droite.  Des
	-- guillemets sont ajoutées autour de la Unbounded_String
	-- Il s'agit d'un masquage de l'opérateur `&` défini dans Strings.Unbounded
	function "&" (Left: String; Right: Unbounded_String) return String is
	begin
		return Left & Avec_Guillemets (Right);
	end;


	-- Surcharge l'opérateur unaire "+" pour convertir une String
	-- en Unbounded_String.
	-- Cette astuce permet de simplifier l'initialisation
	-- de cles un peu plus loin.
	function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;
	function "-" (Item : in Unbounded_String) return String
		renames To_String;

   procedure Afficher (IPtest : T_Adresse_IP ; Masque : T_Adresse_IP; Inter : Unbounded_String) is      --afficher les routes sur demande
      Texte : Unbounded_String;
      IPtemp : Unbounded_String;
      Masquetemp : Unbounded_String;
   begin
      IPtemp := deparse_ip(IPtest);
      Masquetemp :=deparse_ip(Masque);
      Texte := IPtemp & " | " & Masquetemp & " | " & Inter;
      Put_Line(-Texte);
   end Afficher;

    L: T_liste ;

	   function comparer (IPtest : T_Adresse_IP; Masque : T_Adresse_IP; IPdest : T_Adresse_IP) return Boolean is    --type entrees  et format
      begin
         if ((IPtest and Masque) = IPdest) then 
            return True;
         else 
            return False;
         end if;
      end comparer;


	procedure AfficherListe is
        new Pour_Chaque_liste(Afficher);

	function TraitIPListe is
        new TraiterIP_liste (comparer,inf);

    function PresCache is
        new Cle_Match_liste (comparer);

    function AjCache is
        new AjouterCache (comparer, inf,Pos_dernier_1);

	-------------Tests--------

	--- Test des fonctions initialiser et Est_Vide_liste
	procedure Test_Initialiser_Vide is
	begin
		Initialiser_liste (L);
		pragma Assert (Est_Vide_liste (L));
	end Test_Initialiser_Vide;

	--- Test de la fonction Taille_liste
	procedure Test_Taille is
	begin
		Initialiser_liste (L);
		pragma Assert (Taille_liste (L) = 0);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		pragma Assert (Taille_liste (L) = 1);
	end Test_Taille;

	--- Test de la fonction Cle_Presente_liste
	procedure Test_CLe_Presente is
	begin
		Initialiser_liste (L);
		pragma Assert (not Cle_Presente (L, parse_ip(+("142.122.12.13"))) );
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		pragma Assert (Cle_Presente (L, parse_ip(+("142.122.12.13"))) );
	end Test_Cle_Presente;

	--- Test de la fonction Supprimer_par_destination_liste
	procedure Test_Supprimer_par_dest is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		pragma Assert (Cle_Presente (L, parse_ip(+("142.122.12.13"))));
		pragma Assert (Cle_Presente (L, parse_ip(+("197.135.127.2"))));
		Supprimer_par_destination_liste (L, parse_ip(+("197.135.127.2")));
		pragma Assert (Cle_Presente (L, parse_ip(+("142.122.12.13"))));
		pragma Assert (not Cle_Presente (L, parse_ip(+("197.135.127.2"))));
	end Test_Supprimer_par_dest;


	procedure Test_La_Donnee_Inter_liste is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		pragma Assert (La_Donnee_Inter_liste (L, parse_ip(+("142.122.12.13")))=+("eth0"));
	end Test_La_Donnee_Inter_liste;

	procedure Test_La_Donnee_Masque_liste is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		pragma Assert (La_Donnee_Masque_liste (L, parse_ip(+("142.122.12.13")))=parse_ip(+("255.255.0.0" )));
	end Test_La_Donnee_Masque_liste;

	procedure Test_Vider_liste is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		Vider_liste(L);
		pragma Assert (Est_Vide_liste (L));	
	end Test_Vider_liste;

	procedure Test_Incrementer_rec is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		Incrementer_rec(L);
	end Test_Incrementer_rec;

	procedure Test_Incrementer_freq_Max_Min is
	begin
		Initialiser_liste (L);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		Enregistrer_liste ( L, parse_ip(+("197.135.127.2")),+("eth1") , parse_ip(+("255.255.255.0")));
		Incrementer_freq (L, parse_ip(+("142.122.12.13")));
		Incrementer_freq (L, parse_ip(+("142.122.12.13")));
		Incrementer_freq (L, parse_ip(+("197.135.127.2")));
		pragma Assert (Max(L)=parse_ip(+("142.122.12.13")));
		pragma Assert (Min(L)=parse_ip(+("197.135.127.2")));
	end Test_Incrementer_freq_Max_Min;

	procedure Test_Ajouter_regle_liste is
	begin 
		Initialiser_liste (L);
		pragma Assert (Taille_liste (L) = 0);
		Enregistrer_liste ( L, parse_ip(+("142.122.12.13")),+("eth0"),parse_ip(+("255.255.0.0")));
		pragma Assert (Taille_liste (L) = 1);
	end Test_Ajouter_regle_liste;

	procedure Test_PresCache is
	begin		
	Initialiser_liste (L);
	Enregistrer_liste ( L, parse_ip(+("142.122.0.0")),+("eth0"),parse_ip(+("255.255.0.0")));
	Enregistrer_liste ( L, parse_ip(+("197.135.127.0")),+("eth1") , parse_ip(+("255.255.255.0")));
	Incrementer_freq (L, parse_ip(+("142.122.12.13")));
	pragma Assert (PresCache (L, parse_ip(+("142.122.23.23"))));
	pragma Assert ( not PresCache (L, parse_ip(+("142.124.23.23"))));
	pragma Assert (PresCache (L, parse_ip(+("197.135.127.23"))));
	pragma Assert ( not PresCache (L, parse_ip(+("197.135.27.254"))));
	end Test_PresCache;

	procedure Test_TraiterIP_liste is
	begin		
	Initialiser_liste (L);
	Enregistrer_liste ( L, parse_ip(+("142.122.0.0")),+("eth0"),parse_ip(+("255.255.0.0")));
	Enregistrer_liste ( L, parse_ip(+("197.135.127.0")),+("eth1") , parse_ip(+("255.255.255.0")));
	Incrementer_freq (L, parse_ip(+("142.122.12.13")));
	pragma Assert (TraitIPListe (L, parse_ip(+("142.122.23.23")))=parse_ip(+("142.122.0.0")));
	pragma Assert ( not PresCache (L, parse_ip(+("142.124.23.23"))));
	pragma Assert (TraitIPListe (L, parse_ip(+("197.135.127.23")))=parse_ip(+("197.135.127.0")));
	pragma Assert ( not PresCache (L, parse_ip(+("197.135.27.254"))));
	end Test_TraiterIP_liste;

	procedure Test_Pour_Chaque_liste is 
	begin
	Initialiser_liste (L);
	Enregistrer_liste ( L, parse_ip(+("142.122.0.0")),+("eth0"),parse_ip(+("255.255.0.0")));
	Enregistrer_liste ( L, parse_ip(+("197.135.127.0")),+("eth1") , parse_ip(+("255.255.255.0")));
	Incrementer_freq (L, parse_ip(+("142.122.12.13")));
	AfficherListe (L);
	end Test_Pour_Chaque_liste;


begin
	Put_Line ("Programme de test du module tr_liste");
	Test_Initialiser_Vide;
	Put_Line ("--------Test des fonctions Initialiser_liste et Est_Vide_liste OK");
	Test_Taille;
	Put_Line ("--------Test de la fonction Taille_liste OK");
	Test_CLe_Presente;
	Put_Line ("--------Test de la fonction Cle_Presente OK");
	Test_Supprimer_par_dest;
	Put_Line ("--------Test de la procedure Supprimer_par_destination_liste OK");
	Test_La_Donnee_Inter_liste;
	Put_Line ("--------Test de la fonction La_Donnee_Inter_liste OK");
	Test_La_Donnee_Masque_liste;
	Put_Line ("--------Test de la fonction La_Donnee_Inter_liste OK");
	Test_Vider_liste;
	Put_Line ("--------Test de la procedure Vider_liste OK");
	Test_Incrementer_freq_Max_Min;
	Put_Line ("--------Test des fonctions Incrementer_freq, Max et Min OK");
	Test_Incrementer_rec;
	Put_Line ("--------Test de la procedure Incrementer_rec OK");
	Test_Ajouter_regle_liste;
	Put_Line ("--------Test de la procedure Ajouter_regle_liste OK");
	Test_PresCache;
	Put_Line ("--------Test de la fonction PresCache OK");
	Test_TraiterIP_liste;
	Put_Line ("--------Test de la fonction TraiterIP_liste OK");
	Test_Pour_Chaque_liste;
	Put_Line ("--------Test de la procedure Pour_Chaque_liste OK");
end test_tr_liste;

