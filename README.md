# TB-R-Shiny: *Mycobacterium Tuberculosis* Duomenų Analizė

![Framework](https://img.shields.io/badge/Framework-R%20Shiny-blue.svg)
![Field](https://img.shields.io/badge/Field-Bioinformatics-success.svg)
![Status](https://img.shields.io/badge/Status-Development-orange.svg)


**TB-R-Shiny** yra interaktyvus įrankis, skirtas EMU (16S ir ITS) rezultatų analizei ir vizualizavimui. Programa leidžia apjungti taksonominius duomenis su vartotojo metaduomenimis, atlikti filtravimą bei generuoti grafikus, palengvinančius *Mycobacterium tuberculosis* komplekso metagenominę analizę be tiesioginio programavimo.

---

## Turinys
- [Įvadas](#įvadas)
- [Diegimas ir paleidimas](#diegimas-ir-paleidimas)
- [Programos struktūra](#programos-struktūra)
- [Funkcionalumas](#funkcionalumas)
- [Terminų žodynas](#terminų-žodynas)
- [Techninė dokumentacija](#techninė-dokumentacija)

---

## Įvadas

**TB-R-Shiny** yra interaktyvi bioinformatikinės analizės platforma, skirta *Mycobacterium tuberculosis* komplekso tyrimams.

Pagrindinis šios programos tikslas – supaprastinti 16S ir ITS metagenominių duomenų analizę, gautą naudojant EMU (Expectation-Maximization Uniqueness) įrankį. Programa leidžia tyrėjams lengvai apjungti taksonominius rezultatus su savo klinikiniais ar eksperimentiniais metaduomenimis ir generuoti publikacijoms tinkamus grafikus nenaudojant programinio kodo.

Pagrindinės galimybės:

- Greitas EMU rezultatų ir metaduomenų įkėlimas bei apjungimas.
- Dinaminis duomenų filtravimas pagal pasirinktus kriterijus.
- Taksonominės sudėties, alfa ir beta įvairovės vizualizavimas.

---

## Naudojimas be R (Windows vartotojams)

Jei savo kompiuteryje neturite įsidiegę R arba norite greito paleidimo būdo, galite naudokite paruoštą „nešiojamą“ (Portable) versiją.

### Instrukcija:
1.  Atsisiųskite failą: **`TB-R-Shiny-Portable.zip`**.
2.  Išarchyvuokite (Extract) atsisiųstą ZIP failą į bet kurį aplanką savo kompiuteryje.
3.  Atidarykite išarchyvuotą aplanką ir dukart spustelėkite failą:
    *   **`run.bat`** (arba `run.bat` / `TB-App.exe` – *priklausomai nuo to, kaip pavadinsite paleidimo failą*).
4.  Programa atsidarys jūsų numatytoje interneto naršyklėje po kelių sekundžių.

---
## Diegimas ir paleidimas

### Reikalavimai
Prieš pradedant, įsitikinkite, kad turite įsidiegę [R](https://www.r-project.org/) (>= 4.1) ir [RStudio](https://posit.co/download/rstudio-desktop/).

### Diegimas
1. Atsisiųskite projektą
   ```bash
   git clone https://github.com/mmartiss/TB_R_shiny.git
   cd TB_R_shiny
   ```
2. Įdiegiame patį BiocManager (jei jo dar nėra)
    ```R
    if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    ```
3. Paketų diegimas
    ```R
    BiocManager::install(c(
    "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", 
    "ape", "readxl", "writexl", "tidyr", "dplyr", "vegan", 
    "phyloseq", "phytools", "ggtree", "ggdendro", "markdown"
    ))
    ```
4. Programos paleidimas
   ```R
   shiny::runApp()
   ```

---

## Programos struktūra
Projektas organizuotas modulių principu, užtikrinant kodo skaitomumą ir lengvą plečiamumą.

```text
├── modules/
│   ├── upload.r           # Duomenų įkėlimo logika ir UI
│   ├── filter.r           # EMU duomenų filtravimo algoritmai
│   ├── metadataFilter.R   # Metaduomenų valdymas
│   └── amplicon.r         # Analizės ir vizualizacijos (Heatmap, Alpha/Beta)
├── app.r                  # Pagrindinis įėjimo taškas
├── ui.r                   # Vartotojo sąsajos aprašymas
├── server.r               # Serverio pusės logika
└── README.md              # Projekto dokumentacija
```

---

## Naudojimosi instrukcija

### 1. Duomenų įkėlimas

Norėdami pradėti analizę, šoniniame meniu pasirinkite ir įkelkite šiuos failus:

*   **PRIVALOMA: EMU rezultatai** (abundance, counts, taxonomy)
    *   Palaikomi `.tsv` ir `.csv` formatai.
    *   **Svarbu:** Jei jūsų faile naudojami kabliataškiai (`;`), prieš spausdami mygtuką pasirinkite atitinkamą skirtuką (Separator) nustatymuose.
*   **PASIRENKAMA: Filogenetinis medis** (iš EMU duomenų bazės)
    *   Palaikomas `.nwk` (Newick) formatas.
*   **PASIRENKAMA: Metaduomenys**
    *   Palaikomi `.tsv`, `.csv` bei `.xlsx` formatai.
    *   *Pastaba:* Griežtų reikalavimų struktūrai nėra, tačiau rekomenduojama, kad mėginių ID sutaptų su nurodytais EMU rezultatuose.

####     Veikimo principas:
1.  Pasirinkę failus, būtinai paspauskite mygtuką **`Load Data`**.
2.  **Duomenų saugumas:** Programa originalių jūsų failų nekeičia ir netrina. Visi veiksmai, filtravimai ir transformacijos atliekami tik kompiuterio operatyviojoje atmintyje, todėl originalūs duomenys išlieka saugūs.

### 2. Filtravimas
* **EMU filtravimas:** Galite filtruoti duomenis pagal santykinį gausumą (relative abundance) arba specifinius taksonus.
* **Metaduomenų filtravimas:** Pasirinkite tik tuos mėginius, kurie atitinka jūsų tyrimo kriterijus (pvz., tik tam tikros vietovės ar metų duomenis).

### 3. Amplicon analizė
Programoje integruoti šie analizės įrankiai:
* 📊 **Bar plots:** Taksonominės sudėties pasiskirstymas.
* 🌡️ **Heatmap:** Gausiausių rūšių palyginimas tarp mėginių.
* 🌱 **Alpha diversity:** Shannon, Simpson indeksų skaičiavimas.
* 🗺️ **Beta diversity:** PCoA arba NMDS ordinacijos grafikai.
* 🧪 **Metadata analysis:** Ryšių tarp biologinių duomenų ir klinikinių faktorių paieška.

---

## 📝 Terminai (Angl. -> Liet.)

| Angliškai | Lietuviškai | Apibrėžimas |
| :--- | :--- | :--- |
| Relative Abundance | Santykinis gausumas | Procentinė taksono dalis visame mėginyje. |
| Metadata | Metaduomenys | Papildoma informacija apie mėginius (pvz. amžius, lytis). |
| Alpha Diversity | Alfa įvairovė | Rūšių įvairovė vieno mėginio viduje. |
| Beta Diversity | Beta įvairovė | Skirtumai tarp skirtingų bendrijų/mėginių. |

---

## Kodo dokumentacija

### Naudojamos bibliotekos
* `shiny`: Vartotojo sąsajai kurti.
* `tidyverse`: Duomenų transformacijai.
* `phyloseq`: Mikrobiomo duomenų analizei.
* `plotly`: Interaktyviems grafikams.

### Veikimo principai
Programa naudoja `reactive` elementus, todėl pasikeitus filtravimo parametrams, visi grafikai persipiešia automatiškai be puslapio perkrovimo.

### Priimti sprendimai
* **Moduliarizacija:** Pasirinkta `Shiny modules` architektūra, kad būtų išvengta "spagečių kodo" viename faile.
* **Filtravimas:** Sprendimas filtruoti duomenis *prieš* vizualizaciją priimtas siekiant optimizuoti programos greitaveiką dirbant su dideliais duomenų kiekiais.

---

💡 *Pastaba: Ši programa vis dar aktyviai vystoma.*
```

### Kas buvo pakeista/pagerinta:
1.  **Vizualinis aiškumas:** Pridėtos piktogramos (emoji), kurios padeda greičiau orientuotis tekste.
2.  **Struktūra:** Pridėtas "Turinys" (Table of Contents), kad būtų lengva naršyti po ilgą dokumentą.
3.  **Diegimo instrukcijos:** Pridėtas konkretus kodo blokas, kaip atsisiųsti ir paleisti programą (tai labai svarbu programuotojams).
4.  **Lentelės:** Terminų žodynas pateiktas tvarkingoje Markdown lentelėje.
5.  **Išsamesnis aprašymas:** Užpildyti skyriai apie filtravimą ir analizę, kad būtų aišku, ką programa iš tiesų daro.
6.  **Techninės detalės:** Pridėtas minimalus reikalaujamų R paketų sąrašas.