
# FLP - Funkcionální projekt - BKG-2-CNF
* Autor: Ondřej Krejčí (xkrejc69)
* 2022

## Implementace
Implementovaný program splňuje všechny požadavky ze zadání. Program načítá vlastní bezkontextovou gramatiku v zadaném formátu a na základě argumentů vytiskne BKG v daném formátu. Byly implementovány 2 algoritmy z opory předmětu TIN, algoritmus 4.5 na odstranění jednoduchých pravidel a algoritmus 4.7 na převedení do Chomského normální formy. Pokud je při zpracování vstupu nebo provádění algoritmu nalezena chyba, tak se program ukončí s popisem chyby.

## Obsah odevzdaného archivu
* `src/` - zdrojové soubory
* `test/` - testovací vstupy, výstupy a skript
* `Makefile`
* `README.md` - dokumentace

### Formát vstupu
Na vstupu se očekává popis gramatiky v daném formátu o délce minimálně 4 řádky, tj. gramatika obsahuje alespoň jedno pravidlo. Při načítání symbolů se jako symbol berou řetězce oddělené čárkami, u kterých se poté kontroluje jestli patří do povolené množiny znaků, pro neterminály je to dle zadání `[A..Z]`, pro terminály je to dle zadání `['a'..z]` a navíc jsou povoleny následující symboly `['+', '-', '*', '/', '(', ')', '[', ']']` (kvůli použití v testech). U pravidel se očekává, že celá levá strana je jeden neterminál a na levé straně jsou jednotlivé znaky samostatné symboly. Provádí se základní kontroly, zda startovní neterminál a symboly v pravidlech jsou definované symboly. Není povoleno aby pravá strana byla prázdná (ze zadání se očekává, že BKG je vlastní a neobsahuje pravidlo `S -> epsilon`).

### Zpracování argumentů
Program vyžaduje zadání alespoň jednoho argumentu, kterým musí být jeden z `-i`, `-1` nebo `-2`. Druhý argument je volitelný, pokud není zadán, čte se vstup ze standardního vstupu, jinak se tento argument bere jako cesta k souboru se vstupem, který se načte. Pokud nejsou zadány, žádné argumenty, tak se vypíše nápověda a program se ukončí (ale argument `-h` se bere jako chyba).

### Algoritmy
Oba algoritmy jsou implementovány podle opory z předmětu TIN. Předpokládá se, že platí omezené dané zadáním. Na načtenou gramatiku je tedy možné přím aplikovat algoritmus 4.5, jelikož se předpokládá, že gramatika je vlastní. Při provádění algoritmu 4.7 se jako první provede algoritmus 4.7, čímž je získána vlastní gramatika bez jednoduchých pravidel. V tomto algoritmu nejsou implementovány kroky řešící pravidlo `S -> epsilon`.

## Testy
Všechny testovací vstupy (`*.txt`) a referenční výstupy (`*.out`) jsou obsažené ve složce `test`. Pro automatizaci testování byl vytvořen jednoduchý Python skript `test.py`, který spustí program pro všechny testovací vstupy a porovná je s referenčními výstupy.

Pro spuštění testů je možné použít `python3 test.py`, případně `make test`.

Pro testování byly použity primárně příklady následující za definicí algoritmu v opoře TIN. Pro algoritmus 4.5 příklad 4.14 a pro algoritmus 4.7 příklad 4.17. Jako další testovací vstup bylo použito cvičení 4.8.12 z opory TIN (bez referenčního řešení).

Další dva testovací vstupy byly převzaty z následujících zdrojů:
* https://courses.engr.illinois.edu/cs373/fa2013/lectures/lec17.pdf
* https://courses.cs.washington.edu/courses/cse322/08au/lec14.pdf

U těchto dvou příkladů byl očekávaný výstup manuálně přepsán, jelikož v přednáškách, ze kterých byly převzaty je použita jiná notace nových neterminálů.
