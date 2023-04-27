Projekt: ECDSA
Predmet: FLP
Autor: Marek Ziska, xziska03@stud.fit.vutbr.cz

Pouzitie: flp22-fun volby [vstup]

	volby: 
		-i nacita Elliptic Curve(EC) a vypise na stdin vnutornu reprezentaciu EC
		-k nacita EC a vypise na stdin vygenerovany par klucov
        -s nacita EC, par klucov, hash a vygeneruje podpis
        -v nacita EC, podpis, verejny kluc, hash a overi ci je podpis spravny/validny
		
	vstup:
		meno suboru, z ktoreho sa nacitava elipticka krivka, ak chyba, cita stdin
		
Vytvořte program, který implementuje ECDSA a umožňuje pro zvolenou
eliptickou křivku (EC) vygenerovat pár klíčů, podepsat hash zprávy a následný podpis zkontrolovat.

Program zahrnuje a podporuje vsetky argumenty, specifikovane v zadani. Jediny chybicka o ktorej viem
ze ju program obsahuje a ktoru som nestihol vyriesit je vypis verejneho kluca, ktory v pripade nerovnakej 
velkosti koordinatov x a y nedoplnuje z lava nuly na vyrovanie ich velkosti
