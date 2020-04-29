# covid19.model.sa2

A model of the COVID-19 pandemic stratified by SA2.


### Synopsis

Basic usage:

```r
S <- simulate_sa2(days_to_simulate = 5)
```

See `?simulate_sa2` for further arguments.



# Installation

## Mac

Enable OpenMP by installing `clang7`: https://github.com/rmacoslib/r-macos-rtools/releases/tag/v3.2.2


# Model notes

The `simulate_sa2` agent-based microsimulation model uses Australian Census data, epidemiological and policy inputs. These elements are described briefly below.


<!-------------------------------------------------------------------------------->
## The `australia` dataset

The model is run on a synthetic unit record dataset of the Australian population. This dataset is constructed using counts of people, workers and households by place of usual residence and place of work in the 2016 Census, accessed via TableBuilder. Schools location and student counts sourced from ACARA. Supermarket locations data is sourced from Google Places.

These datasets are detailed below.

### Households

### People

### Non-private dwellings

Source: '2016 Census - Counting Persons, Place of Enumeration (MB)',
accessed via TableBuilder.

Variables:

- `SA1`
- `NPDD Type of Non-Private Dwelling`, in Grattan-defined categories:
    - Hospital:
        - Public hospital (not psychiatric)
        - Private hospital (not psychiatric)
        - Psychiatric hospital or institution
    - Care:
        - Hostel for the disabled
        - Nursing home
        - Accommodation for the retired or aged (not self-contained)
    - Prison:
        - Corrective institution for children
        - Prison, corrective institution for adults
    - Other non-private:
        - Hotel, motel, bed and breakfast
        - Nurses' quarters
        - Staff quarters
        - Boarding house, private hotel
        - Boarding school
        - Residential college, hall of residence
        - Hostel for homeless, night shelter, refuge
        - Childcare institution 
        - Other welfare institution
        - Immigration detention centre
        - Convent, monastery, etc.
        - Other and non-classifiable
    - Private:
        - Not stated
        - Not applicable

### Workplaces

**Data**: SA2 place of usual residence and workplace Destination Zone.
**Source**: '2016 Census - Counting Employed Persons, Place of Work (POW)'

**Variables**:

* `SA2 (UR)`
* `DZN (POW)`



### Schools

**Data**: ACARA schools data

School locations (`school-locations.xlsx`) and profiles (`school-profile.xlsx`)
sourced from ACARA's Data Access Program: https://www.acara.edu.au/contact-us/acara-data-access.








<!-------------------------------------------------------------------------------->
## Epidemiological input


From Imperial College London, R_0 under certain intervention (_see covid19model repo and results_):

* R0 [0.7, 1.3] post lockdown (e.g. Germany)
* pR0 [0, 0.3] social distancing (0 no effect, 1 elimination)
* pR0 [0.5, 0.8] lockdown (i.e. only essential excursions permitted)

The simulation takes epidemiological values through the `set_epipars()` function. The defaults are:

```r
set_epipars <- function(incubation_distribution = c("pois", "lnorm", "dirac"),
                        incubation_mean = 5,
                        incubation_sigma = 0.44,
                        illness_distribution = c("pois", "lnorm", "dirac"),
                        illness_mean = 15,
                        illness_sigma = 1,
                        r_distribution = c("cauchy", "lnorm", "pois", "dirac"),
                        r_location = 2,
                        r_scale = 1,
                        p_asympto = 0.48,
                        p_critical = 0.02,
                        p_death = 0.01)
```

These defaults are described below.

### Incubation: `incubation_distribution`, `incubation_mean`, `incubation_sigma`

Each person who is infected with COVID-19 in the simulation is given an incubation period that is drawn from the incubation distribution with a defined mean and sigma.

By default, this is a Poisson distribution with mean `5` days and sigma `0.44`, in line with parameters used by the [Imperial College COVID-19 Response Team](https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf).

[_note:_ justification for distribution]

[_note:_ graphic of distribution with defaults and with plausible alternative]


### Illness: `illness_distribution`, `illness_mean`, `illness_sigma`

Each person who is infected with COVID-19 and becomes symptomatic in the simulation is given an illness period that is drawn from the illness distribution with a defined mean and sigma.

By default, this is a Poisson distribution with mean `15` days and sigma `1`. 

[_note:_ citations for defaults; justification for distribution]

[_note:_ graphic of distribution with defaults and with plausible alternative]


### Reproduction number: `r_distribution`, `r_location`, `r_sigma`

The reproduction number distribution is, by default, a Cauchy distribution with location 2 and scale 1. In circumstances that require a reproduction number -- e.g. when there is an infected person in a supermarket, or at a school -- a figure is drawn that determines the number of people they go onto infect.



[_note:_ citations for defaults; justification for distribution]

[_note:_ graphic of distribution with defaults and with plausible alternative]


### Probability of reaction to COVID-19: `p_asympto`, `p_critical`, `p_death`

The probability of symptom severity is taken from findings gathered by the [Imperial College COVID-19 Response Team](https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf)

[_note:_ many citations]


### Resistance

An individual's resistance to a COVID-19 infection relates to their chance of being infected when they are in close contact with someone.

This resistance number factors in a person's carefulness with spatial distancing, frequency of washing hands, wearing a mask, etc.

A person's resistance in the model is randomly drawn a uniform distribution between from `1` (more likely to be infected) to `1000` (least likely to be infected).

The resistance score is used with two parameters:

- `resistance1`: the threshold below which an infection takes place. By default, this is set to 400. 
- `resistance_penalty`: a value is added to the `resistance1` threshold, increasing the likelihood of infection in the household. The value is drawn from a uniform distribution between 0 and `resistency_penalty` for each household. `resistance_penalty` has a default value of `100`.
- `resistance2`: used with a person's `Age` to increase the liklihood of both active and critical cases among the elderly.


### Children and COVID-19 

How children spread the virus differently to adults is important, particularly when it comes to school closures. But the evidence is limited. Summary and some international literature below, but this is a developing field.

- Children appear to spread it less than adults -- both to other children and to adults. 
- Children are also less likely to be the index case in a household than adults, and than children in otherwise similar viruses. 
- Definitely _get_ it less than adults. 
- Children are not likely to be asymptomatic. In China: (~2-5% of confirmed cases, _decreasing_ with age); are most likely to have mild (~50%) or moderate (30%) symptoms [Dong et al]. Zero asymptomatic children found in Iceland or Netherlands. 

Our base could be the [NSW study](http://ncirs.org.au/sites/default/files/2020-04/NCIRS%20NSW%20Schools%20COVID_Summary_FINAL%20public_26%20April%202020.pdf), released this week: 

- In NSW, from March to mid-April 2020, 18 individuals (9 students and 9 staff) from 15 schools were confirmed as
COVID-19 cases; all of these individuals had an opportunity to transmit the COVID-19 virus (SARS-CoV-2) to
others in their schools.
- 735 students and 128 staff were close contacts of these initial 18 cases.
- No teacher or staff member contracted COVID-19 from any of the initial school cases.
- One child from a primary school and one child from a high school may have contracted COVID-19 from the initial
cases at their schools

The international literature is below.

### Bi et al: Shenzhen, China
Bi, Q., Wu, Y., Mei, S., Ye, C., Zou, X., Zhang, Z., … Feng, T. (2020). Epidemiology and transmission of COVID-19 in 391 cases and 1286 of their close contacts in Shenzhen, China: a retrospective cohort study. The Lancet Infectious Diseases. https://doi.org/10.1016/S1473-3099(20)30287-5

**Date:** 27 April
**Data:** From Jan 14 to Feb 12, 2020, the Shenzhen Center for Disease Control and Prevention identified 391 SARS-CoV-2 cases and 1286 close contacts. 

> Cases were older than the general population (mean age 45 years) and balanced between males (n=187) and females (n=204). 356 (91%) of 391 cases had mild or moderate clinical severity at initial assessment. As of Feb 22, 2020, three cases had died and 225 had recovered (median time to recovery 21 days; 95% CI 20–22). Cases were isolated on average 4·6 days (95% CI 4·1–5·0) after developing symptoms; contact tracing reduced this by 1·9 days (95% CI 1·1–2·7). Household contacts and those travelling with a case were at higher risk of infection (odds ratio 6·27 [95% CI 1·49–26·33] for household contacts and 7·06 [1·43–34·91] for those travelling with a case) than other close contacts. **The household secondary attack rate was 11·2% (95% CI 9·1–13·8), and children were as likely to be infected as adults (infection rate 7·4% in children <10 years vs population average of 6·6%)**. The observed reproductive number (R) was 0·4 (95% CI 0·3–0·5), with a mean serial interval of 6·3 days (95% CI 5·2–7·6).




### Danis et al: French Alps case study
Danis, K., Epaulard, O., Bénet, T., Gaymard, A., Campoy, S., Bothelo-Nevers, E., … Team, I. (2020). Cluster of coronavirus disease 2019 (Covid-19) in the French Alps, 2020. Clinical Infectious Diseases. https://doi.org/10.1093/cid/ciaa424


**Date:** 11 April
**Data:** index case study starting in French Alps


> The index case stayed 4 days in the chalet with 10 English tourists and a family of 5 French residents; SARS-CoV-2 was detected in 5 individuals in France, 6 in England (including the index case), and 1 in Spain (overall attack rate in the chalet: 75%). **One pediatric case, with picornavirus and influenza A coinfection, visited 3 different schools while symptomatic**. One case was asymptomatic, with similar viral load as that of a symptomatic case. Seven days after the first cases were diagnosed, one tertiary case was detected in a symptomatic patient with a positive endotracheal aspirate; all previous and concurrent nasopharyngeal specimens were negative. Additionally, 172 contacts were monitored, including 73 tested negative for SARS-CoV-2.

> The occurrence in this cluster of one asymptomatic case with similar viral load as a symptomatic patient, suggests transmission potential of asymptomatic individuals. **The fact that an infected child did not transmit the disease despite close interactions within schools suggests potential different transmission dynamics in children.** Finally, the dissociation between upper and lower respiratory tract results underscores the need for close monitoring of the clinical evolution of suspect Covid-19 cases.


### Zhu et al: household clusters internationally
Zhu, Y., Bloxham, C. J., Hulme, K. D., Sinclair, J. E., Wei, Z., Tong, M., … Short, K. R. (2020). Children are unlikely to have been the primary source of household SARS-CoV-2 infections. https://doi.org/10.1101/2020.03.26.20044826

**Date:** 30 March

**Data:** 31 households with clusers from international literature


> Drawing on studies from China, Singapore, South Korea, Japan, and Iran a broad range of clinical symptoms were observed in children. These ranged from asymptomatic to severe disease. **Of the 31 household transmission clusters that were identified, 9.7% (3/31) were identified as having a paediatric index case.** This is in contrast other zoonotic infections (namely H5N1 influenza virus) where 54% (30/56) of transmission clusters identified children as the index case.

> Whilst SARS-CoV-2 can cause mild disease in children, **the data available to date suggests that children have not played a substantive role in the intra-household transmission of SARS-CoV-2**.




### Ministry of Health: Netherlands
National Institute for Public Health and the Environment. (2020). Children and COVID-19.

**Date:** April
**Data:** admin and surveys of Dutch households

> RIVM research into the reports shows that the spread of COVID-19 mainly takes place between persons of approximately the same age. This is based on data from patients with symptoms and concerns people between 40 and 80 years. Children are less likely to be infected by adults. When this does happen, it turns out that it is mainly in the home situation. Patients under 20 years play a much smaller role in the spread than adults and the elderly.

> Around 40 GP practices in the Netherlands register the number of patients visiting the practice with flu-like complaints, through the NIVEL testing stations . A culture from the nose and throat is taken from some of these patients, which is examined in the laboratory to detect viruses, such as COVID-19. In total,  6.5% of them turned out to be infected. This percentage was highest in week 14 with 30%. In the last weeks the percentage was about 15-20%. **No infection was found in the patients under 20 years of age who were tested. This confirms the current understanding that children are less likely to be infected and become ill than adults**.

> RIVM National Institute for Public Health and the Environment  set up a study in a short period to find out more about COVID-19 infected people and their family contacts. Families participate in this study in collaboration with GGD Utrecht. Up until mid April, a total of 54 households with a total of 239 participants, 185 of whom are housemates are participating. This involves 123 adults and 116 children between the ages of 1 and 16. Although the study is still ongoing, there are already preliminary results. **There are no indications that children younger than 12 years were the first to be infected within the family. Children who were found to be infected with COVID-19 had fewer symptoms than adults. Respiratory complaints, such as sore throats, coughs and nasal colds, were also less common in children than in adults.**



### Gudbjartsson et al: Iceland
Gudbjartsson, D. F., Helgason, A., Jonsson, H., Magnusson, O. T., Melsted, P., Norddahl, G. L., … Stefansson, K. (2020). Spread of SARS-CoV-2 in the Icelandic Population. New England Journal of Medicine, NEJMoa2006100. https://doi.org/10.1056/NEJMoa2006100

> As of April 4, a total of 1221 of 9199 persons (13.3%) who were recruited for tar- geted testing had positive results for infection with SARS-CoV-2. Of those tested in the general population, 87 (0.8%) in the open-invitation screening and 13 (0.6%) in the random-population screening tested positive for the virus.

> Children under 10 years of age were less likely to receive a positive result than were persons 10 years of age or older, with percentages of 6.7% and 13.7%, respectively, for targeted testing; in the population screening, no child under 10 years of age had a positive result, as compared with 0.8% of those 10 years of age or older.


**Key:** There is no evidence out of Iceland of asymptomatic children with COVID-19. Random sampling of the population found that children (under 20) were less likely to be COVID-19 positive than adults, with no children under 10 testing positive.





### Dong et al: China
Dong Y, Mo XI, Hu Y, et al. Epidemiological characteristics of 2143 pediatric patients with 2019 coronavirus disease in China. Pediatrics. 2020;16:16



Children are not likely to be asymptomatic (~2-5% of confirmed cases, _decreasing_ with age); most likely to have mild (~50%) or moderate (30%) symptoms.


Table (2)
Age Group,    | Asymptomatic, n (%) | Mild, n (%) | Moderate, n (%) | Severe, n (%) | Critical, n (%) | Total, n |
------------- |-------------------- | ----------- | --------------- | ------------- | --------------- | --------- |
<1            | 7 (1.9)             | 204 (54.2)  | 125 (33.2)      | 33 (8.8)      | 7 (1.9)         | 376 |
1–5           | 15 (3.1)            | 245 (49.9)  | 195 (39.7)      | 34 (6.9)      | 2 (0.4)         | 491 |
6–10          | 30 (5.8)            | 277 (53.3)  | 191 (36.7)      | 22 (4.2)      | 0 (0.0)         | 520 |
11–15         | 27 (6.5)            | 198 (48.1)  | 170 (41.3)      | 14 (3.4)      | 3 (0.7)         | 412 |
\>15          | 15 (4.5)            | 164 (49.1)  | 145 (43.4)      | 9 (2.7)       | 1 (0.3)         | 334 |
Total         | 94 (4.4)            | 1088 (51.0) | 826 (38.7)      | 112 (5.3)     | 13 (0.6)        | 2133 |






### Qiu et al: Zhejiang, China
Qiu, H., Wu, J., Hong, L., Luo, Y., Song, Q., and Chen, D. (2020). **Clinical and epidemiological features of 36 children with coronavirus disease 2019 (COVID-19) in Zhejiang, China: an observational cohort study. The Lancet Infectious Diseases**. https://doi.org/10.1016/S1473-3099(20)30198-5

**Date**: 25 March

**Data:** Paediatric patients (aged 0–16 years) with confirmed COVID-19 from electronic medical records in three hospitals in Zhejiang, China.

**Key:** Large proportion of children with COVID-19 are asymptomatic. The route of transmission for almost all children was by close contact with family members.


> The route of transmission was by close contact with family members (32 [89%]) or a history of exposure to the epidemic area (12 [33%]); eight (22%) patients had both exposures

> Although all paediatric patients in our cohort had mild or moderate type of COVID-19, the large proportion of asymptomatic children indicates the difficulty in identifying paediatric patients who do not have clear epidemiological information, leading to a dangerous situation in community-acquired infections.


### Lee et al: comment
Lee, P. I., Hu, Y. L., Chen, P. Y., Huang, Y. C., and Hsueh, P. R. (2020). Are children less susceptible to COVID-19? Journal of Microbiology, Immunology and Infection. https://doi.org/10.1016/j.jmii.2020.02.011

**Date:** February 25

**Data:** None

> The reasons for the relative resistance of children to some infectious diseases remains obscure. It was suggested that maturational changes in the axonal transport system may explain the relative resistance of immature mice to poliovirus-induced paralysis. Other suggested reasons include children having a more active innate immune response, healthier respiratory tracts because they have not been exposed to as much cigarette smoke and air pollution as adults, and fewer underlying disorders. A more vigorous immune response in adults may also explain a detrimental immune response that is associated with acute respiratory distress syndrome.




### Kelvin & Halperin: comment
Kelvin, A. A., and Halperin, S. (2020). COVID-19 in children: the link in the transmission chain. The Lancet Infectious Diseases. https://doi.org/10.1016/S1473-3099(20)30236-X

**Date:** 25 March

**Data:** None

**Key:** Children can get COVID-19 and are often symptomatic, meaning they could be facilitators of the virus.


> Infants and young children are typically at high risk for admission to hospital after respiratory tract infection with viruses such as respiratory syncytial virus and influenza virus. Immaturity of the respiratory tract and immune system is thought to contribute to severe viral respiratory disease in this age group. Therefore, the absence of paediatric patients with COVID-19 has perplexed clinicians, epidemiologists, and scientists. Case definitions and management strategies for children are absent because of the limited number of paediatric patients with COVID-19.

> The most important finding to come from [Qiu et al] is the clear evidence that children are susceptible to SARS-CoV-2 infection, but frequently do not have notable disease, raising the possibility that children could be facilitators of viral transmission. If children are important in viral transmission and amplification, social and public health policies (eg, avoiding interaction with elderly people) could be established to slow transmission and protect vulnerable populations. There is an urgent need to for further investigation of the role children have in the chain of transmission.




### Observations around transmission locations

> Background: By early April 2020, the COVID-19 pandemic had infected nearly one million people and had spread to nearly all countries worldwide. It is essential to understand where and how SARS-CoV-2 is transmitted. Methods: Case reports were extracted from the local Municipal Health Commissions of 320 prefectural cities (municipalities) in China, not including Hubei province, between 4 January and 11 February 2020. We identified all outbreaks involving three or more cases and reviewed the major characteristics of the enclosed spaces in which the outbreaks were reported and associated indoor environmental issues. Results: Three hundred and eighteen outbreaks with three or more cases were identified, involving 1245 confirmed cases in 120 prefectural cities. We divided the venues in which the outbreaks occurred into six categories: homes, transport, food, entertainment, shopping, and miscellaneous. Among the identified outbreaks, 53.8% involved three cases, 26.4% involved four cases, and only 1.6% involved ten or more cases. **Home outbreaks were the dominant category (254 of 318 outbreaks; 79.9%), followed by transport (108; 34.0%; note that many outbreaks involved more than one venue category).** Most home outbreaks involved three to five cases. We identified only a single outbreak in an outdoor environment, which involved two cases. Conclusions: All identified outbreaks of three or more cases occurred in an indoor environment, which confirms that sharing indoor space is a major SARS-CoV-2 infection risk.

https://www.medrxiv.org/content/10.1101/2020.04.04.20053058v1

## Policies

Policy parameters are set by the `set_policypars()` function. The default values are:

```r
set_policypars <- function(supermarkets_open = TRUE,
                           schools_open = FALSE,
                           only_Year12 = FALSE,
                           do_contact_tracing = TRUE,
                           contact_tracing_days_before_test = 0L,
                           contact_tracing_days_until_result = 3L,
                           contact_tracing_only_sympto = TRUE,
                           tests_by_state = NULL,
                           max_persons_per_event = 5L,
                           max_persons_per_supermarket = 200L)
```



**Supermarkets** can be `open` or `closed`. If supermarkets are closed, people do not make any trips to the supermarket.

_NOTE_: can we decrease the number of trips people take to the supermarket? eg limit to one per week under a particular scenario?


**Schools** can be:

- `open`: on weekdays, all students go to school.
- `only_Year12`: on weekdays, only students in Year 12 go to school
- `closed`: no students go to school. 

_NOTE_: should model opening X days per week, like NSW is doing now?

**Contact tracing** can be performed or not. If there is contact tracing:

- `contact_tracing_days_before_test` sets the number of days following the end of the incubation period before the person gets tested; 
- `contact_tracing_days_until_result` sets the number of days between a test and the result being known;
- `contact_tracing_only_sympto` determines whether contact is only applied to symptomatic cases.

The number of **tests** each state performs is set by `tests_by_state`.

The number of people allowed at **gatherings** is set by `max_persons_per_event` and defaults to five. The maximum number of people allowed in a supermarket is set by `max_persons_per_supermarket` and defaults to 200 people (effectively no limit). 

These policy settings affect the workings of the simulation events, described below.


<!---------------------------------------------------------------------------->
## Simulation

The `australia` dataset is loaded. Each observation is a person (`pid`) who lives in a house (`hid`). Each house is in an 'statistical area' (SA2) about the size of a postcode. The house can contain other people. Aggregated to the SA2 level, the make up of the households --- lone person, couple with or without kids, group household, etc --- reflects the Australian Census.

Each person has an age, determined by whether they are an adult or a child. 
If the person is a child, they can attend a school in their SA2 on weekdays.

If they are an adult and they are in the labour force, they have a job located in a 'destination zone', an area about the size of a city block in a CBD, and larger in areas with more sparsely populated commerce.

Each adult visits the supermarket a certain number of times per year.

The simulation starts on day zero with each person given one of six COVID-19 statuses: 

- `-2` Dead
- `-1` Healed
- `0` Susceptible
- `1` Infected and in their incubation period, not showing symptoms
- `2` Infected and ill, showing symptoms
- `3` Critical

The starting point of the simulation is set by the user, and defaults to the number of dead, healed, critical and infected people by state. The number of infected people who are asymptomatic is determined by the epidemiological inputs. The number of susceptible people is the rest of the population (i.e. there is assumed to be no immunity.)

A night, each person goes back to their house and has contact with other members.

In the morning, each person wakes up and has their COVID-19 status re-assessed. 


### Morning COVID-19 status assessment

If a person is infected, their COVID-19 status can remain the same, get plausibly better or plausibly worse:

- an infected asymptomatic person `1` can:
    - `-1` become healed, i.e. this person has remained asymptomatic for the duration of their illness.
    - `1` remain asymptomatic
    - `2` become symptomatic
- an infected symptomatic person `2` can:
    - `-1` become healed
    - `2` remain symptomatic
    - `3` become critical
- a critical person `3` can:
    - `-2` die
    - `-1` become healed
    - `3` remain critical


To what status the infected individual moves will depend on the number of days since their infection and conditions drawn from the epidemiology inputs.

With the daily status of everyone in the country determined, some people head to the supermarket.


### The supermarket

If the supermarket is open (see policy parameters), some adults will go to a supermarket nearby their home on a given day. How often they go to the supermarket is random for each person, ranging uniformly from 1 to 365 times per year. 

Supermarkets have 8 hours of operation, and people are randomly assigned an hour to shop in.

The number of infectious people in a supermarket in a particular hour defines how many additional infections occur, based on the reproduction number distribution (see the epidemiological varibales). The reproduction number distribution is reduced to reflect limited contact within supermarkets.

_Note_: Hugh to explain the `1/32` reduction. Has noted the model is sensitive to this.

A number of symptomatic people, equal to the number of additional infections, with low enough resistance will be infected. 

If supermarkets are closed, nobody goes to the supermarket. 

### Work 

_Workplace interactions have not yet been added to the model_.



### School

If schools are open (see policy parameters) and it is a weekday, each school-age child will attend a nearby school.

The number of kids infected by each infectious person in a school is drawn from the reproduction number distribution, set by the epidemiological variables.

If school is closed, this action is skipped. If school is only open to Year 12s, only kids aged 17-18 attend school. 


### House

Each member of a household returns to their house at the end of the day.

There is no transmission in a single person household.

Transmission can occur in a house with more than one person with at least one person infected. A person's resistance determines their likelihood of being infected by another household memmber (see epidemiological variables).

People then go to bed, before waking up and doing the same thing over again.


## Model output

The model returns the original `australia` dataset 
