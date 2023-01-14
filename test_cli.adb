with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions;            use Ada.Exceptions;
with SDA_Exceptions;            use SDA_Exceptions;	
with cli ; use cli;

procedure test_cli is
    RegleS : Unbounded_String;
    MasqueS : Unbounded_String;
    InterfaceS : Unbounded_String;
    IPS : Unbounded_String;
    IPvf : T_Adresse_IP;
    Ip2 : T_Adresse_IP;
    Masque : T_Adresse_IP;

    function "+" (Item : in String) return Unbounded_String
        renames To_Unbounded_String;
    function "-" (Item : in Unbounded_String) return String
        renames To_String;

    procedure Test_parse_regle is 
    begin 
        Regles := +"122.312.312.323 122.2.2.9 eth2";
        parse_regle(Regles, IPS, MasqueS, InterfaceS);
        pragma Assert(IPS = +"122.312.312.323");
        pragma Assert(MasqueS = +"122.2.2.9");
        pragma Assert(InterfaceS = +"eth2");
    end Test_parse_regle;

    procedure Test_deparse_IP is 
    begin 
        IPS := +"122.312.312.323";
        IPvf := parse_IP(IPS);
        pragma Assert (IPS = deparse_ip(IPvf));
    end Test_deparse_IP;

    procedure Test_inf is
    begin
        Ipvf := parse_ip(+"122.312.312.323");
        Ip2 := parse_ip(+"122.312.312.324");
        pragma Assert (inf(Ipvf, Ip2));
    end Test_inf;

    procedure Test_Pos_dernier_1 is
        compteur: Integer;
    begin   
        Ipvf := parse_ip(+"255.255.0.0");
        compteur := Pos_dernier_1(Ipvf);
        pragma Assert (compteur = 16);
    end Test_Pos_dernier_1;

    procedure Test_construire_masque is
        compteur: Integer;
    begin
        compteur := 15;
        Masque := construire_masque(compteur);
        pragma Assert (Masque = +parse_ip(+"255.254.0.0"));
    end Test_construire_masque;
    
begin
	Put_Line ("Programme de test du module CLI");
    Test_parse_regle;
    Put_Line ("--------Test de la procedure Parse_regle OK");
    Test_deparse_IP;
    Put_Line ("--------Test de la procedure Deparse_IP OK");
    Test_inf;
    Put_Line ("--------Test de la procedure Inf OK");
    Test_Pos_dernier_1;
    Put_Line ("--------Test de la procedure Pos_dernier_1 OK");
    Test_construire_masque;
    Put_Line ("--------Test de la procedure Construire_masque OK");


end test_cli;
