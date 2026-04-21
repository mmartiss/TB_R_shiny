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

1. `File upload` - Duomenų įkėlimas
2. `Abundance filtering` - EMU duomenų filtravimas
3. `Counts filtering` - EMU duomenų filtravimas
4. `Taxonomy filtering` - EMU duomenų filtravimas
5. `Metadata filtering` - Metaduomenų filtravimas
6. `Amplicon`


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
1. Pasirinkite EMU failų skirtuką. `.tsv`, `.csv`, `;`
2. Jei duomenys buvo keisti ir pirmoji eilutė nėra stulpelių pavadinimai, atžymėkite varnelę `First row is header`. Šitaip pirmasis stulpelis nebus traktuojamas kaip antraštė ir bus paemami duomenys.
1.  Pasirinkę failus, būtinai paspauskite mygtuką **`Load Data`**.
2.  **Duomenų saugumas:** Programa originalių jūsų failų nekeičia ir netrina. Visi veiksmai, filtravimai ir transformacijos atliekami tik kompiuterio operatyviojoje atmintyje, todėl originalūs duomenys išlieka saugūs.

### 2. Filtravimas

Duomenų filtravimas NEPRIVALOMAS. Jei nenorite keisti ar tikrinti įkeltų duomenų galite tiesiai eiti prie 6 punkto `Amplicon`.

Programą sudaro du pagrindiniai moduliai: `filter` ir `metadataFilter`. 

### 2.1. EMU duomenų filtravimo modulis (`filter`)
Šis modulis yra universalus ir programoje naudojamas **tris kartus** kaip trys nepriklausomi filtrai šiems duomenų rinkiniams:
1.  **Santykinis gausumas (Abundance)** - `Abundance filtering`
2.  **Skaitymų skaičius (Counts)** - `Counts filtering`
3.  **Taksonomija (Taxonomy)** - `Taxonomy filtering`

#### Lentelės struktūros valdymas
Šios funkcijos keičia pačią lentelės formą:
*   **Column Removal:** Galimybė pašalinti nereikalingus stulpelius (pvz., ištrinti mėginius, kurie nebus naudojami analizėje).
*   **Sort:** Duomenų rūšiavimas pagal bet kurį pasirinktą stulpelį didėjimo arba mažėjimo tvarka.
*   **Rename Column:** Interaktyvus stulpelių pavadinimų keitimas (patogu ruošiant grafikus).
*   **Batch Replace:** Vertės keitimo funkcija visame stulpelyje (pvz., pakeisti "NA" į "0" arba pervadinti specifines vertes).

#### Filtravimo mechanizmai
Modulis palaiko „filtravimo istoriją“ – galite dėti kelis filtrus vieną ant kito, o vėliau juos po vieną pašalinti per „Active filters“ ženkliukus.

*   **Column-Specific Filter:** 
    *   *Skaitiniams duomenims:* Slankiklis (Slider) rėžiams nustatyti (pvz., rodyti tik tuos, kurių vertė nuo 10 iki 100).
    *   *Tekstiniams duomenims:* Pasirinkimo sąrašas (Dropdown) konkrečioms vertėms atsirinkti.
*   **Global Value Filter:** Filtruoja visą lentelę iš karto. Galima nustatyti taisyklę (pvz., palikti eilutę, jei *bent viename* stulpelyje vertė > 0.05). Palaiko tekstinį (contains, starts with) ir skaitinį režimus.
*   **Min Total Reads Filter:** Pašalina taksonus, kurių bendra suma per visus mėginius nesiekia nustatytos ribos.

#### Bioinformatinės funkcijos
Specifinės funkcijos, pritaikytos mikrobiomo duomenims:
*   **Taxonomy Join:** Iš įkelto taksonomijos failo, funkcija pagal `tax_id` prijungia rūšies pavadinimą ir sukuria `Species_Full` stulpelį (Genus + Species).
*   **Group by Taxonomy:** Duomenų agregavimas. Galite sumuoti visus skaitymus pasirinktame lygmenyje (pvz., sumuoti visas rūšis į šeimų (family) ar genčių (genus) lygmenį).

#### Eksportas ir valdymas
*   **Reset All:** Vieno paspaudimu grįžtama prie pradinių įkeltų duomenų.
*   **Eksportas:** Apdorotą lentelę bet kuriame etape galima atsisiųsti `.csv` arba `.tsv` formatu.
*   **Use for Analysis:** Patvirtina galutinį duomenų rinkinį, kuris bus naudojamas grafikų braižymui.
    - Šį mygtuką paspausti reikia tik tuo atvėju, kai norite, kad pritaikyti filtrai ir naujai gauta lentelė būtų naudojama grafikuose. Jei norėsite grafikuose vaikščioti tarp originalių ir keistų duomenų failų rekomenduojame filtruotą failą išsaugoti. Jį reikės įkelti programos pradžioje vietoje originalaus EMU failo. `Use for Analysis` mygtuko dar kartą spausti nereikės.

---

### 2.2. Metaduomenų filtravimo modulis (`metadataFilter.R`)

Šis modulis leidžia paruošti metaduomenų lentelę analizei naudojant šias funkcijas:

####  Mėginių atranka (Filtravimas)
*   **Stulpelio filtras:** Eilučių atranka pagal pasirinkto stulpelio reikšmes paspaudus `Add Filter`.
*   **Globalus filtras:** Reikšmių paieška per visus stulpelius vienu metu (tekstinė arba skaitinė) paspaudus `Apply Global Filter`.
*   **Filtrų valdymas:** Aktyvių filtrų peržiūra ir jų šalinimas paspaudus `X` ikoną šalia filtro pavadinimo.

#### Duomenų redagavimas
*   **Reikšmių keitimas:** Masinis pasirinkto stulpelio verčių keitimas paspaudus `Replace All`.
*   **Naujų stulpelių kūrimas:** Galimybė pridėti naują stulpelį paspaudus `Add New Column`.
    - Užpildytą fiksuota reikšme - `Fixed value`
    - Tuščias stulpelis - `Empty (NA)`
    - Eilučių numeracija - `Row numbers`
    - Kito stulpelio kopija - `Copy from column`
*   **Tiesioginis redagavimas:** Galimybė keisti reikšmes tiesiogiai lentelės langeliuose (Editable cells).

#### Stulpelių ir eilučių valdymas
*   **Šalinimas:** Pasirinktų stulpelių pašalinimas paspaudus `Remove Selected`.
*   **Pervadinimas:** Stulpelio pavadinimo keitimas paspaudus `Rename`.
*   **Rūšiavimas:** Lentelės rikiavimas pagal pasirinktą stulpelį paspaudus `Sort`.
    - Didėjimo (Asc.) arba mažėjimo (Desc.) tvarka

#### Rezultatai ir eksportas
*   **Atstatymas:** Grįžimas prie pradinės lentelės būsenos paspaudus `Reset Metadata`.
*   **Eksportas:** Paruoštos lentelės atsisiuntimas paspaudus `Download CSV` arba `Download Excel`.
*   **Patvirtinimas:** Duomenų perdavimas į analizės modulius paspaudus `Confirm Metadata for Analysis`.

---



































### 🛠️ Techninė filtravimo logika
*   **Reaktyvumas:** Filtrai sukurti naudojant `reactiveValues` ir `observeEvent` funkcijas, todėl pakeitimai viename modulyje gali daryti įtaką visai programos būsenai realiu laiku.
*   **Duomenų vientisumas:** Filtravimo metu sukuriami nauji duomenų objektai, todėl pirminiai duomenys išlieka nepakitę iki sesijos pabaigos.

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