# Logicky Projekt 2 FLP 2022/23 - Turingov stroj

Autor: Bc. Marek Ziska
Login: xziska03
Mail: xziska03@stud.fit.vutbr.cz

## Implementacia simulatora nedeterministickeho turingova stroje v jazyku Prolog.

### Popis zo zadania:

- Vaším úkolem je vytvořit simulátor nedeterministického
  Turingova stroje. Na vstupu váš program obdrží pravidla pro
  Turingův stroj a vstupní obsah pásky. Výstupem bude
  posloupnost konfigurací stroje.
- Vnitřní stavy stroje jsou označeny velkými písmeny,
  vstupní/páskovou abecedu tvoří malá písmena, prázdný
  symbol je mezera, počáteční stav je „S“, koncový stav je „F“.
- Výpočet stroje končí přechodem do koncového stavu nebo abnormálním zastavením. Pravidla jsou
  zadána ve tvaru <stav> <symbol na pásce> <nový stav> <nový symbol na pásce nebo „L“, „R“>.
- Jednotlivé části jsou odděleny mezerou, každé pravidlo bude na samostatném řádku. Symboly L/R
  značí posun hlavy doleva/doprava. Na posledním řádku vstupu je uveden vstupní obsah pásky
  (nekonečná posloupnost prázdných symbolů na konci pásky není uváděna).
- Jednotlivé konfigurace stroje vypisujte na samostatné řádky. Konfiguraci uvádějte v pořadí <obsah
  pásky před hlavou><stav><symbol pod hlavou><zbytek pásky> (bez oddělovačů).
  Zaměřte se na korektní vstupy a situace, kdy existuje sekvence přechodů do koncového stavu.
- V takovém případě Váš program musí toto řešení najít. Implementační detaily v případě abnormálního
  zastavení nebo cyklení jsou ponechány na autorovi.

### Spustenie:

    make
    ./flp22-log < /cesta_k_testum/vstup1.txt > /cesta_k_vysledkum/vystup1.txt

### Testovacie vstupy:

- vstup1.txt - referencny test zo zadania
- vstup2.txt - test na vyber koncove stavu pri moznosti viacerich konfiguracii pre rovnaky stav a symbol na paske
- vstup3.txt - test na odolnost TS pri posunu vlavo na lavej hranici pasky
- vstup4.txt - test na odolnost TS pri posunu vpravo na pravej hranici pasky
- vstup5.txt - test na schopnost prace s prazdnymi znakmi
