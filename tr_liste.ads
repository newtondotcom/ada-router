-- Définition de structures de données associatives sous forme d'une liste
-- chaînée associative (LCA).
generic
	type T_IP is private;
	type T_Masque is private;
	type T_Interface is private;

package  TR_liste is

	type T_liste is limited private;

	-- Initialiser une Liste chaînée associative
	procedure Initialiser_liste(Sda: out T_liste) with
		Post => Est_Vide_liste (Sda);


	-- Est-ce qu'une Sda est vide ?
	function Est_Vide_liste (Sda : T_liste) return Boolean;

	-- Est que Ip est présente dans la liste
	function Cle_Presente(Sda : T_liste ; IP : T_IP) return Boolean;

	-- Obtenir le nombre d'éléments dans une Sda.
	function Taille_liste (Sda : in T_liste) return Integer with
		Post => Taille_liste'Result >= 0
			and (Taille_liste'Result = 0) = Est_Vide_liste (Sda);


	-- Enregistrer des données dans une Sda.
	procedure Enregistrer_liste (Sda : in out T_liste ; IP : in T_IP ; Interfac : in T_Interface ; Masque : in T_IP) with
	Post => Cle_Presente (Sda, IP) and La_Donnee_Inter_liste (Sda, IP) = Interfac and La_Donnee_Masque_liste (Sda, IP) = Masque
			and Taille_liste (Sda) = Taille_liste(Sda)'Old  + 1 and not Est_Vide_liste (Sda) and (not Cle_Presente(Sda, IP)'Old);

	-- Supprimer une donnée repérée par son IP dans une Sda.
	procedure Supprimer_par_destination_liste (Sda : in out T_liste ; Destination : in T_IP) with
		Pre => not Est_Vide_liste (Sda) and (Taille_liste (Sda) > 0) and Cle_Presente(Sda, Destination),
		Post => (Taille_liste (Sda) = Taille_liste(Sda)'Old  - 1) and (not Cle_Presente(Sda, Destination));

	-- Obtenir l'interface associée à une IP dans la Sda.
	-- Exception : Cle_Absente_Exception, si la clé n'est pas présente.
	function La_Donnee_Inter_liste (Sda : in T_liste ; Destination : in T_IP) return T_Interface;

	-- Obtenir le masque associé à une IP dans la Sda.
	-- Exception : Cle_Absente_Exception, si la clé n'est pas présente.
	function La_Donnee_Masque_liste (Sda : in T_liste ; Destination : in T_IP) return T_IP;

	-- Vider la liste
	procedure Vider_liste (Sda : in out T_liste) with
     Post => Est_Vide_liste (Sda);

	-- Incrémenter le compteur de toutes les règles de la table de routage afin de savoir laquelle a été la moins récemment utilisée
	procedure Incrementer_rec (Sda : in T_liste) ; 

	-- Incrémenter le compteur d'une règle en la repérant par son IP (comparaison avec le et logique)
	procedure Incrementer_freq (Sda : in T_liste ; IP : T_IP) ;

	-- Renvoyer l'adresse ip de la règle de l'interface dont le compteur associé est le plus grand
	function Max (Sda : in T_liste) return T_IP with
		Pre => not Est_Vide_liste (Sda) and (Taille_liste (Sda) > 0);

	-- Renvoyer l'adresse ip de la règle de l'interface dont le compteur associé est le plus petit
	function Min (Sda : in T_liste ) return T_IP with
		Pre => not Est_Vide_liste(Sda) and (Taille_liste (Sda) > 0);
		
	-- Supprimer la règle située à la fin de la liste (en queue)
	procedure Supprimer_fin(Sda : in out T_liste) with
		Pre => not Est_Vide_liste (Sda) and Taille_liste (Sda) > 0,
		Post => Taille_liste (Sda) = Taille_liste(Sda)'Old  - 1;

	---Ajouter une règle au début de la liste (en tête)
	procedure Ajouter_regle_liste(Sda : in out T_liste ; IP : in T_IP ; Interfac : in T_Interface ; Masque : in T_IP) with
	Post => Cle_Presente (Sda, IP) and La_Donnee_Inter_liste (Sda, IP) = Interfac and La_Donnee_Masque_liste (Sda, IP) = Masque
			and Taille_liste (Sda) = Taille_liste(Sda)'Old  + 1 and not Est_Vide_liste (Sda) and (not Cle_Presente(Sda, IP)'Old);
	
   -- Renvoie la dernière adresse IP de destination de la liste où il y a un match avec IPentree
   	generic
		with function comparer (IPtest : T_IP; Masque : T_IP; IPdest : T_IP) return Boolean; -- return (IPtest and Masque = IPdest)
		with function inf (Masque1 : T_IP; Masque2 : T_IP) return Boolean; ----- return (Masque1 < Masque2)
   function TraiterIP_liste(Sdad : in T_liste; IPentree : in T_IP) return T_IP  with
		Pre => not Est_Vide_liste (Sdad); 

	-- Savoir si la fonction TraiterIP_liste renverra une règle correspondante présente dans le cache
   	generic
		with function comparer (IPtest : T_IP; Masque : T_IP; IPdest : T_IP) return Boolean;
	function  Cle_Match_liste (Sda : in T_liste ; Destination : in T_IP) return Boolean;

	-- Appliquer un traitement (Traiter) pour chaque couple d'une Sda.
	generic
		with  procedure Afficher (IPtest : T_IP ; Masque :T_IP ; Inter : T_Interface);
	procedure Pour_Chaque_liste (Sda : in T_liste);


	-- Affiche la position du dernier 1 en partant de la gauche de l'IP match de IPentree écrit en binaire, dont le masque est l'avant dernier le plus long
   	generic
		with function comparer (IPtest : T_IP; Masque : T_IP; IPdest : T_IP) return Boolean; -- return (IPtest and Masque = IPdest)
		with function inf (Masque1 : T_IP; Masque2 : T_IP) return Boolean; ----- return (Masque1 < Masque2)
		with function Pos_dernier_1(Masque : in T_IP) return Integer; ---- return (position du dernier 1 dans Masque)
   function AjouterCache(Sdad : in T_liste; IPentree : in T_IP) return Integer; ----Cache

private

	type T_Cellule;

	-- Création du type T_liste, pointant vers un maillon de la liste chaînée
	type T_liste is access T_Cellule;

	-- Création du type T_Cellule, pour faire un encapsulage technique des informations techniques
	type T_Regle is 
		record 
			IP : T_IP ;
			Masque : T_IP ;
			interfac : T_Interface;
			end record;

	-- Création du type T_Cellule, un maillon de la liste chaînée
	type T_Cellule is
		record
			Regle : T_Regle;
			Compteur : Integer;
			Suivant : T_liste;
         	end record;

end TR_liste;
