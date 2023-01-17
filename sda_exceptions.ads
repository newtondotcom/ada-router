-- Définition d'une exception commune à toutes les SDA.
package SDA_Exceptions is

   Cle_Absente_Exception  : Exception;	-- une clé est absente d'un SDA
   Position_occupee : Exception; -- une clé est déjà présente dans un SDA avec table de hachage
   Nouvelle_Exception: Exception;
   Regle_non_trouvee: Exception;
   Lecture_point : Exception;

   
end SDA_Exceptions;