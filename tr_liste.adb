with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with SDA_Exceptions;            use SDA_Exceptions;

package body TR_liste is

	procedure Free_liste is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_liste);


	procedure Initialiser_liste(Sda: out T_liste) is
	begin
		Sda:=Null;
	end Initialiser_liste;


	function Est_Vide_liste (Sda : T_liste) return Boolean is
	begin
         return (Sda = null);
	end Est_Vide_liste;


	function Taille_liste (Sda : in T_liste) return Integer is
	begin
	  if Sda = Null then
		return 0;
	  else
		return 1 + Taille_liste (Sda.all.Suivant);
	  end if;
	end Taille_liste;


     procedure Enregistrer_liste (Sda : in out T_liste ; IP : in T_IP ; Interfac : in T_Interface ; Masque : in T_IP) is
          Nouv_Cellule : T_liste ;
          Regleb : T_Regle;
     begin
          if Est_Vide_liste(Sda) then
               Regleb.interfac := Interfac ;
               Regleb.IP := IP;
               Regleb.Masque :=  Masque ;
               Nouv_Cellule := new T_Cellule'(Regleb, 0 ,Sda);
               Sda := Nouv_Cellule;
          else
               Enregistrer_liste(Sda.all.Suivant, IP, Interfac , Masque);
          end if;
     end Enregistrer_liste;

   function La_Donnee_Inter_liste (Sda : in T_liste ; Destination : in T_IP) return T_Interface is
   begin
         if Est_Vide_liste(Sda) then
            raise Cle_Absente_Exception;
         elsif Sda.all.Regle.IP = Destination then
            return Sda.Regle.interfac;
         else
            return La_Donnee_Inter_liste(Sda.all.suivant,Destination);
         end if;
   end La_Donnee_Inter_liste;
   
   function La_Donnee_Masque_liste (Sda : in T_liste ; Destination : in T_IP) return T_IP is
   begin
         if Est_Vide_liste(Sda) then
            raise Cle_Absente_Exception;
         elsif Sda.all.Regle.IP = Destination then
            return Sda.all.Regle.Masque;
         else
            return La_Donnee_Masque_liste(Sda.all.suivant,Destination);
         end if;
   end La_Donnee_Masque_liste;


   procedure Supprimer_par_destination_liste (Sda : in out T_liste ; Destination : in T_IP) is
      Miroir :  T_liste;
   begin
         if Est_Vide_liste(Sda) then
            Null;
         elsif Sda.all.Regle.IP = Destination then
            Miroir:=Sda;
            Sda:=Sda.all.Suivant;
            Free_liste(Miroir);
         else
            Supprimer_par_destination_liste(Sda.all.Suivant, Destination);
         end if;  
   end Supprimer_par_destination_liste;


   procedure Vider_liste (Sda : in out T_liste) is
      Miroir :  T_liste := Sda;
   begin
      if Est_Vide_liste(Sda) then
         Null;
      else
         Miroir:=Sda;
         Sda:=Sda.all.Suivant;   
         Free_liste(Miroir);
         Vider_liste(Sda);
      end if;
   end Vider_liste;

	procedure Incrementer_rec (Sda : in T_liste) is
   Miroir : T_liste := Sda ;
   begin
      if Est_Vide_liste(Miroir) then
         Null;
      else 
         Miroir.all.Compteur := Miroir.all.Compteur +1 ; 
         Miroir := Miroir.all.Suivant;
      end if;
   end Incrementer_rec;


	procedure Incrementer_freq (Sda : in T_liste ; IP : T_IP) is 
   Miroir : T_liste := Sda ;
   begin
      while Est_Vide_liste(Miroir) loop
         if IP = Miroir.all.Regle.IP then
            Miroir.all.Compteur := Miroir.all.Compteur +1 ; 
         end if;
         Miroir := Miroir.all.Suivant ;
      end loop;
   end Incrementer_freq;

	function Max (Sda : in T_liste) return T_IP is  
   compteur_temp : Integer := Sda.all.Compteur ;
   ip_temp : T_IP := Sda.all.Regle.IP;
   Miroir : T_liste := Sda ;
   begin
      while Miroir.all.Suivant /= Null loop
         if Miroir.all.Compteur > compteur_temp then 
            compteur_temp := Miroir.all.Compteur ; 
            ip_temp := Miroir.all.Regle.IP;
         end if;
         Miroir := Miroir.all.Suivant ; 
      end loop;
      return ip_temp ;
   end Max;

	function Min (Sda : in T_liste) return T_IP is
   compteur_temp : Integer := Sda.all.Compteur ;
   ip_temp : T_IP := Sda.all.Regle.IP;
   Miroir : T_liste := Sda ;
   begin
      while Miroir.all.Suivant /= Null loop
         if Miroir.all.Compteur < compteur_temp then 
            compteur_temp := Miroir.all.Compteur ; 
            ip_temp := Miroir.all.Regle.IP;
         end if;
         Miroir := Miroir.all.Suivant ; 
      end loop;
      return ip_temp ;
   end Min;

   procedure Ajouter_regle_liste(Sda : in out T_liste ; IP : in T_IP ; Interfac : in T_Interface ; Masque : in T_IP) is
      Nouv_Cellule : T_liste ;
      Regleb : T_Regle;
      begin
         Regleb.interfac := Interfac ;
         Regleb.IP := IP;
         Regleb.Masque :=  Masque ;
         Nouv_Cellule := new T_Cellule'(Regle => Regleb, Compteur => 0, Suivant => Sda);
         Sda := Nouv_Cellule;
	end Ajouter_regle_liste;

   function TraiterIP_liste(Sdad : in T_liste; IPentree : in T_IP) return T_IP is
   MasqueTemp : T_IP;
   IPtemp : T_IP;
   Miroir : T_liste := Sdad;
   Premiere : Boolean := False; ---savoir si une adresse a déjà été affectée à MasqueTemp
   begin
      for I in 1..Taille_liste(Sdad) loop
         if not Premiere then
               if comparer(IPentree, Miroir.all.Regle.Masque, Miroir.all.Regle.IP) then
                  Premiere := True;
                  MasqueTemp := Miroir.all.Regle.Masque ;
                  IPtemp := Miroir.all.Regle.IP ;
               end if;
         else 
            if comparer(IPentree, Miroir.all.Regle.Masque, Miroir.all.Regle.IP) and inf(MasqueTemp,Miroir.all.Regle.Masque) then
               MasqueTemp := Miroir.all.Regle.Masque ; 
               IPtemp := Miroir.all.Regle.IP ;
            end if;
         end if;   
         Miroir := Miroir.all.Suivant;
      end loop; 
      return IPtemp;    
   end TraiterIP_liste;

	function Cle_Match_liste (Sda : in T_liste ; Destination : in T_IP) return Boolean is 
   begin
         if Sda = Null then
            return False;
         elsif comparer(Destination, Sda.all.Regle.Masque, Sda.all.Regle.IP) then
            return True ;
         elsif Sda.all.Suivant /= Null then
            return Cle_Match_liste(Sda.all.Suivant, Destination);
         end if;
        return False;
   end Cle_Match_liste;

   procedure Pour_Chaque_liste (Sda : in T_liste) is
   begin
      if Sda=Null then
         Null;
      else
         Afficher(Sda.all.Regle.IP, Sda.all.Regle.Masque, Sda.all.Regle.interfac);
         Pour_Chaque_liste(Sda.all.Suivant);
      end if;
   end Pour_Chaque_liste ;

   procedure Supprimer_fin(Sda : in out T_liste) is
   begin
      if Sda.all.Suivant = Null then
         Free_liste(Sda);
      else
         Supprimer_fin(Sda.all.Suivant);
      end if;
      
   end Supprimer_fin;

   function Cle_Presente(Sda : T_liste ; IP : T_IP) return Boolean is
   begin
      if Sda = Null then
         return False;
      elsif Sda.all.Regle.IP = IP then
         return True ;
      elsif Sda.all.Suivant /= Null then
         return Cle_Presente(Sda.all.Suivant, IP);
      end if;
      return False;
   end Cle_Presente;

   function AjouterCache(Sdad : in T_liste; IPentree : in T_IP) return Integer is
      IPtempM : T_IP;
      MasqueTempM : T_IP;
      IPtemp : T_IP;
      MasqueTemp : T_IP;
      PremiereM : Boolean := False; ---savoir si une adresse a déjà été affectée à MasqueTempM et IPtempM
      Premiere : Boolean := False; ---savoir si une adresse a déjà été affectée à MasqueTemp et IPtemp
      Miroir : T_liste := Sdad;
      compteur : Integer;
   begin
      for I in 1..Taille_liste(Sdad) loop
         if not Premiere then
               if comparer(IPentree, Miroir.all.Regle.Masque, Miroir.all.Regle.IP) then
                  MasqueTemp := Miroir.all.Regle.Masque ;
                  IPtemp := Miroir.all.Regle.IP ;
                  Premiere := True;
               end if;
         Put_Line("Premiere");
         else 
            if comparer(IPentree, Miroir.all.Regle.Masque, Miroir.all.Regle.IP) and inf(MasqueTemp,Miroir.all.Regle.Masque) then
               if not PremiereM then
                  if inf(MasqueTemp, Miroir.all.Regle.Masque) then 
                     MasqueTempM := Miroir.all.Regle.Masque ; 
                     IPtempM := Miroir.all.Regle.IP ;
                  else
                     MasqueTempM := MasqueTemp;
                     IPtempM := IPtemp;
                     MasqueTemp := Miroir.all.Regle.Masque ; 
                     IPtemp := Miroir.all.Regle.IP ;
                  end if;
                  PremiereM := True;
               elsif comparer(IPentree, Miroir.all.Regle.Masque, Miroir.all.Regle.IP) and inf(MasqueTempM,Miroir.all.Regle.Masque) then
                  if inf(MasqueTempM,MasqueTemp) then
                     MasqueTempM := MasqueTemp;
                     IPtempM := IPtemp;
                     MasqueTemp := Miroir.all.Regle.Masque ; 
                     IPtemp := Miroir.all.Regle.IP ;
                  else 
                     MasqueTempM := Miroir.all.Regle.Masque ; 
                     IPtempM := Miroir.all.Regle.IP ;
                  end if;
               end if;
            end if;
         end if;   
         Miroir := Miroir.all.Suivant;
        end loop; 
        if not PremiereM then
         compteur := Pos_dernier_1(IPtempM);
         else
         compteur := Pos_dernier_1(IPtemp);
         end if;
        return compteur;  
   end AjouterCache;

end TR_liste;
