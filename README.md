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
1. Atsisiųsti projektą
   ```bash
   git clone https://github.com/mmartiss/TB_R_shiny.git
   cd TB_R_shiny
   ```
2. Įdiegti BiocManager (jei jo dar nėra)
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

Pasirinkimai: `Abundance filtering`, `Counts filtering`, `Taxonomy filtering`. 

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
*   **Reset All:** Vienu paspaudimu grįžtama prie pradinių įkeltų duomenų.
*   **Eksportas:** Apdorotą lentelę bet kuriame etape galima atsisiųsti `.csv` arba `.tsv` formatu.
*   **Use for Analysis:** Patvirtina galutinį duomenų rinkinį, kuris bus naudojamas grafikų braižymui.
    - Šį mygtuką paspausti reikia tik tuo atveju, kai norite, kad pritaikyti filtrai ir naujai gauta lentelė būtų naudojama grafikuose. Jei norėsite grafikuose vaikščioti tarp originalių ir keistų duomenų failų, rekomenduojame filtruotą failą išsaugoti. Jį reikės įkelti programos pradžioje vietoje originalaus EMU failo. `Use for Analysis` mygtuko dar kartą spausti nereikės.

---

### 2.2. Metaduomenų filtravimo modulis (`metadataFilter.R`)

Pasirinkimas: `Metadata filtering`.

Šis modulis leidžia paruošti metaduomenų lentelę analizei naudojant šias funkcijas:

####  Mėginių atranka (Filtravimas)
*   **Stulpelio filtras:** Eilučių atranka pagal pasirinkto stulpelio reikšmes paspaudus `Add Filter`.
*   **Globalus filtras:** Reikšmių paieška per visus stulpelius vienu metu (teksto arba skaičių) paspaudus `Apply Global Filter`.
*   **Filtrų valdymas:** Aktyvių filtrų peržiūra ir jų šalinimas paspaudus `X` ikoną šalia filtro pavadinimo.

#### Duomenų redagavimas
*   **Reikšmių keitimas:** Masinis pasirinkto stulpelio verčių keitimas paspaudus `Replace All`.
*   **Naujų stulpelių kūrimas:** Galimybė pridėti naują stulpelį paspaudus `Add New Column`.
    - Užpildytas fiksuota reikšme - `Fixed value`
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

### 3. Amplicon analizė

- Konfigūracija: Pasirinkite norimus parametrus viršutiniame skydelyje.
- Vykdymas: Būtina paspausti žalią mygtuką `Update All Analysis`. Tik jį paspaudus programa atliks skaičiavimus ir sugeneruos/atnaujins visų kortelių (Bar Chart, Heatmap, Alpha, Beta, Metadata) grafikus.


### 3.1 Pagrindiniai nustatymai

- Taxonomic level: Pasirenkamas taksonominis gylis (nuo species iki phylum), kuriame bus atliekama analizė.
- Top N taxa: Nurodomas gausiausių taksonų skaičius (pvz., 20), kurie bus vaizduojami atskirai. Visi kiti mažesnio gausumo taksonai bus sugrupuoti į kategoriją „Other“.
- Color palette: Parenkama spalvų paletė taksonams atvaizduoti (pvz., Tableau 10, Pastel, Viridis).
- Sort Samples By: Nustatoma mėginių rikiavimo tvarka visuose grafikuose:
  - Alphabetical – pagal pavadinimą.
  - Dendrogram – pagal mėginių panašumą (cluster analizė), bar plot ir heatmap prideda medį.
  - Shannon / Observed – pagal bioįvairovės rodiklius.
  - Bray-Curtis / Jaccard / UniFrac – pagal pasirinktą beta įvairovės metriką.
- Sample Type Filter: Leidžia greitai atfiltruoti mėginius pagal mėginio pavadinimo pabaigą (pvz., tik -S tipo arba tik -T tipo mėginiai).

### 3.2 Taxonomic Bar Chart (Taksonominė sudėtis)
Tai pagrindinis grafikas, rodantis santykinį bakterijų gausumą (procentais).
- Ką galima daryti: Matyti, kaip pasiskirsto dominuojančios rūšys kiekviename mėginyje.
- Valdymas:
  - Spalvų keitimas: Virš grafiko atsiranda spalvų sąrašas su pavadinimais. Spustelėkite ant spalvos kvadrato ir pasirinkite naują spalvą – grafikas atsinaujins realiu laiku.
  - Slinkimas: Grafikas yra labai platus (2000px), todėl jį galite slinkti į šonus (horizontal scroll), kad matytumėte visus mėginius aiškiai.
- Svarbu: Jei pagrindiniuose nustatymuose (viršuje) pasirinkote `Sort Samples By`: `Dendrogram`, po stulpeliais bus nubraižytas mėginių panašumo medis.


### 3.3 Heatmap
Vizualizuoja taksonų gausumą spalvų skale.
- Ką galima daryti: Identifikuoti taksonus, kurie būdingi tik tam tikroms mėginių grupėms.
- Valdymas: Naudoja tuos pačius „Top N“ taksonus, nustatytus viršutiniame skydelyje.
- Svarbu: Kaip ir stulpelinėje diagramoje, čia gali būti rodoma dendrograma, padedanti grupuoti panašius mėginius.



### 3.4 Alpha Diversity (Alfa įvairovė)

- Ką galima daryti: Palyginti skirtingų grupių bioįvairovės indeksus.
- Valdymas:
  - Metrics: Pažymėkite varneles prie Observed, Shannon, Simpson ar InvSimpson, kad pamatytumėte skirtingus skaičiavimo metodus.
  - Grupavimas: Laukelyje Group By (Metadata) pasirinkite stulpelį iš savo metaduomenų (pvz., liga), kad mėginiai būtų sugrupuoti į Boxplots.
  - Sub-filtravimas: Laukelyje Select values galite atsirinkti tik tam tikras grupes (pvz., palikti tik „Control“ grupę).
- Reikalavimas: Grupavimo funkcijos neveiks be įkeltų metaduomenų failo.

### 3.5 Beta Diversity (Beta įvairovė)
Rodo mėginių tarpusavio panašumą PCoA (Principal Coordinates Analysis) grafike.
- Ką galima daryti: Matyti, ar mėginiai formuoja grupes (klasterius) pagal panašumą.
- Valdymas:
  - Distance Metric: Pasirinkite metodą (Pvz. Bray-Curtis, Jaccard). UniFrac pasirinkimai veiks tik jei įkėlėte filogenetinį medį.
  - Color Samples By: Nuspalvinkite taškus pagal:
    - Metadata Category (reikia metaduomenų).
    - Taxon Abundance – nuspalvina mėginius pagal jūsų pasirinkto taksono kiekį (pasirinkite taksoną `Select Taxon` laukelyje).
    - Dominant Taxon – nuspalvina pagal tai, kokia bakterija tame mėginyje gausiausia.
- Reikalavimas: UniFrac atstumams būtinas .nwk medis.

### 3.6 Metadata Analysis (Metaduomenų analizė)
Grafinis taksonų sudėties vidurkis jūsų pasirinktose grupėse.
- Ką galima daryti: Matyti ne kiekvieną mėginį atskirai, o bendrą vaizdą pagal kategorijas.
- Valdymas:
  - X Axis (Category): Pasirinkite metaduomenų stulpelį, kuris bus apačioje (pvz., amžiaus grupė). Programa susumuos visų tos grupės mėginių vidurkius.
  - Facet Row / Facet Column: Galimybė suskaidyti grafiką į langelius pagal papildomus metaduomenis (pvz., viršuje - tepinėlis, šone – TB tipas).
  - Show individual samples: Pažymėkite, jei norite matyti ne vidurkį, o visus tos grupės mėginius, sudėtus vieną šalia kito.
  - Keep in: Galite pasirinkti, kurias konkrečias kategorijų vertes įtraukti į grafiką.
- Reikalavimas: Ši kortelė bus visiškai tuščia, jei neįkėlėte metaduomenų failo.

---

## Terminai (Angl. -> Liet.)

### Duomenų valdymas ir struktūra
| Angliškai | Lietuviškai | Apibrėžimas |
| :--- | :--- | :--- |
| **Remove Column** | Stulpelio šalinimas | Pasirinkto stulpelio (pvz., nereikalingo mėginio) ištrynimas iš lentelės. |
| **Rename Column** | Stulpelio pervadinimas | Galimybė suteikti naują pavadinimą esamam stulpeliui. |
| **Sort By** | Rūšiuoti pagal | Eilučių rikiavimas didėjimo arba mažėjimo tvarka pagal pasirinktą stulpelį. |
| **Batch Replace** | Masinis keitimas | Funkcija, leidžianti vienu metu visas senas vertes pakeisti naujomis. |
| **Add New Column** | Naujo stulpelio pridėjimas | Naujo tuščio arba užpildyto stulpelio įterpimas į lentelę. |
| **Fixed Value** | Fiksuota reikšmė | Viena konstanta (tekstas ar skaičius), kuria užpildomas visas stulpelis. |
| **Row Numbers** | Eilučių numeracija | Automatinis stulpelio sugeneravimas su eilučių eilės numeriais. |
| **Copy from Column** | Kopijuoti iš stulpelio | Naujo stulpelio sukūrimas dubliuojant kito stulpelio duomenis. |
| **Load Data** | Įkelti duomenis | Mygtukas, kuris aktyvuoja pasirinktų failų nuskaitymą į atmintį. |

### Filtravimas
| Angliškai | Lietuviškai | Apibrėžimas |
| :--- | :--- | :--- |
| **Global Value Filter** | Globalus reikšmių filtras | Funkcija, ieškanti reikšmės visuose stulpeliuose vienu metu. |
| **Global Ignore** | Globalus ignoravimas | Stulpelių pasirinkimas, kurie nebus įtraukti į globalią paiešką. |
| **Filter by Specific Column** | Filtras pagal stulpelį | Vieno konkretaus stulpelio atrinkimas |
| **Any column matches** | Atitinka bet kuris | Logika: palikti eilutę, jei *bent viename* stulpelyje rasta reikšmė. |
| **All columns match** | Atitinka visi | Logika: palikti eilutę tik jei *visuose* stulpeliuose rasta reikšmė. |
| **Min Total Reads** | Min. bendras skaitymų kiekis | Riba, nuo kurios pašalina taksonus, kurių suma per visus mėginius yra per maža. |
| **Taxonomy Join** | Taksonomijos prijungimas | Funkcija, sujungianti gausumo lentelę su taksonomijos aprašymais. |
| **Group & Sum Counts** | Grupuoti ir sumuoti | Duomenų agregavimas į aukštesnį lygį (pvz., visų rūšių sumavimas į genties lygį). |
| **Active Filters** | Aktyvūs filtrai | Sąrašas šiuo metu pritaikytų taisyklių, kurias galima atšaukti. |

### Amplikonų analizė
| Angliškai | Lietuviškai | Apibrėžimas |
| :--- | :--- | :--- |
| **Taxonomic Level** | Taksonominis lygmuo | Klasifikacijos gylis (rūšis, gentis, šeima, būrys, klasė, tipas). |
| **Top N Taxa** | N gausiausių taksonų | Apribojimas rodyti tik tam tikrą skaičių dominuojančių bakterijų. |
| **Sample Type Filter** | Mėginių tipo filtras | Greita atranka pagal mėginių pavadinimų galūnes (pvz., `-S` arba `-T`). |
| **Sort Samples By** | Rūšiuoti mėginius pagal | Mėginių išdėstymo tvarka grafike (pagal abėcėlę, įvairovę ar panašumą). |
| **Alpha Diversity** | Alfa įvairovė | Vieno mėginio vidinė rūšių įvairovė |
| **Beta Diversity** | Beta įvairovė | Sudėties skirtumai tarp skirtingų mėginių (panašumo analizė). |
| **Color Editor** | Spalvų redaktorius | Interaktyvus įrankis, leidžiantis keisti kiekvieno taksono spalvą grafike. |
| **Distance Metric** | Atstumo metrika | Matematinis būdas apskaičiuoti skirtumą tarp mėginių (pvz., Bray-Curtis). |
| **PCoA** | Pagrindinių koordinačių analizė | Beta įvairovės vizualizavimo būdas 2D erdvėje. |
| **Dominant Taxon** | Dominuojantis taksonas | Gausiausia bakterijų rūšis konkrečiame mėginyje. |
| **Facet Row/Column** | Skaidymas eilutėmis/stulpeliais | Grafiko suskaidymas į langelius pagal metaduomenų kategorijas. |
| **Show Individual Samples** | Rodyti pavienius mėginius | Nustatymas, ar rodyti grupės vidurkį, ar kiekvieną mėginį atskirai. |
| **Reset All** | Atstatyti viską | Visų pritaikytų filtrų ir pakeitimų anuliavimas. |
| **Use for Analysis** | Naudoti analizei | Patvirtinimas, kad apdoroti duomenys yra paruošti grafikų braižymui. |

---

## Kodo dokumentacija

### Naudojamos bibliotekos
- `shiny`
- `shinydashboard`
- `shinyWidgets`
- `DT`
- `plotly`
- `ape`
- `readxl`
- `writexl`
- `tidyr`
- `dplyr`
- `vegan`
- `phyloseq`
- `phytools`
- `ggtree`
- `ggdendro`
- `markdown`


### Techniniai sprendimai ir algoritmai

### 1. Bioįvairovės skaičiavimai
Programoje naudojami standartiniai bioinformatikos metodai, realizuoti per specializuotas bibliotekas:

*   **Alfa įvairovė (Alpha Diversity):**
    *   **Shannon, Simpson, InvSimpson:** Skaičiuojami naudojant `vegan::diversity()` funkciją.
    *   **Observed:** Skaičiuojamas unikalių taksonų kiekis eilutėje (`rowSums(mat > 0)`).
*   **Beta įvairovė (Beta Diversity):**
    *   **Bray-Curtis ir Jaccard:** Skaičiuojami naudojant `vegan::vegdist()`. Jaccard metrikai naudojamas nustatymas `binary = TRUE`.
    *   **Weighted ir Unweighted UniFrac:** Skaičiuojami naudojant `phyloseq::UniFrac()`. Tai evoliuciniu atstumu grįstos metrikos, kurioms būtinas filogenetinis medis.

### 2. Dendrogramos ir rūšiavimas
Kai pasirenkate rūšiavimą pagal **Dendrogram**, programa atlieka šiuos žingsnius:
1.  **Atstumų matrica:** Apskaičiuojama pagal pasirinktą metriką (pvz., Bray-Curtis).
2.  **Klasterizacija:** Naudojama **Ward.D2** metodika (`hclust(dist_mat, method = "ward.D2")`). Šis metodas minimizuoja dispersiją klasterių viduje, todėl grupės grafiškai atrodo labai aiškiai.
3.  **Duomenų gavyba:** Naudojamas `ggdendro::dendro_data()`, kad medžio struktūra būtų paversta koordinatėmis, kurias gali atvaizduoti `plotly`.

### 3. Filogenetinio medžio apdorojimas
Įkeltas `.nwk` failas (`data_res$tree`) nėra tiesiogiai braižomas, jis naudojamas kaip skaičiavimų pagrindas:
*   **Suderinimas:** Naudojant `ape::keep.tip()`, medis apkarpomas taip, kad jame liktų tik tie taksonai, kurie egzistuoja jūsų gausumo lentelėje.
*   **Šaknies nustatymas:** Naudojamas `phytools::midpoint.root()`. Tai kritiškai svarbu, nes UniFrac algoritmas reikalauja, kad medis būtų su šaknimi (rooted).
*   **Struktūros korekcija:** Naudojama `ape::multi2di()`, kuri išskleidžia politomijas (daugiašakius mazgus) į dichotominę struktūrą, kad skaičiavimai būtų stabilūs.

### 4. Grafinis atvaizdavimas
Visi programos grafikai yra sugeneruoti derinant dvi technologijas:
*   **Variklis:** `ggplot2` naudojamas sudėtingoms diagramoms (Boxplots, PCoA, Facets) konstruoti.
*   **Interaktyvumas:** Galutiniai `ggplot2` objektai paverčiami į interaktyvius elementus naudojant `plotly::ggplotly()`. Tai leidžia vartotojui:
    *   Priartinti grafiko dalis (zoom).
    *   Užėjus su pele pamatyti tikslias vertes (Tooltip).
    *   Atsisiųsti grafiką kaip `.png` paveikslėlį tiesiai iš naršyklės.

### 5. Duomenų transformacija
*   **Long format:** Gausumo duomenys viduje paverčiami iš „plačios“ lentelės (kur stulpeliai yra mėginiai) į „ilgą“ formatą (`tidyr::pivot_longer`), kad būtų galima efektyviai filtruoti ir grupuoti duomenis pagal metaduomenis.
*   **Relative Abundance:** Santykinis gausumas skaičiuojamas kiekvieną kartą generuojant grafiką (`counts / sum(counts)`), todėl vartotojas gali laisvai keisti taksonominį lygmenį, o programa automatiškai perskaičiuoja procentus.

---

---
