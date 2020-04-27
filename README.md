# covid19.model.sa2

A model of the COVID-19 pandemic stratified by SA2.


[intro]

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
  Other non-private:
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

- `SA2 (UR)`
- `DZN (POW)`



### Schools

**Data**: ACARA schools data

School locations (`school-locations.xlsx`) and profiles (`school-profile.xlsx`)
sourced from ACARA's Data Access Program: https://www.acara.edu.au/contact-us/acara-data-access.








<!-------------------------------------------------------------------------------->
## Epidemiological input


From Imperial College London, R_0 under certain intervention (_see covid19model repo and results_):

- R0 [0.7, 1.3] post lockdown (e.g. Germany)
- pR0 [0, 0.3] social distancing (0 no effect, 1 elimination)
- pR0 [0.5, 0.8] lockdown (i.e. only essential excursions permitted)

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

[citations for defaults and distribution]

[graphic of distribution with defaults and with plausible alternative]


### Illness: `illness_distribution`, `illness_mean`, `illness_sigma`


[citations for defaults and distribution]

[graphic of distribution with defaults and with plausible alternative]


### Reproduction number: `r_distribution`, `r_location`, `r_sigma`

[citations for defaults and distribution]

[graphic of distribution with defaults and with plausible alternative]


### Probability of reaction to COVID-19: `p_asympto`, `p_critical`, `p_death`

[citations]


### Resistance

An individual's resistance to a COVID-19 infection relates to their chance of being infected when they are in close contact with someone.

This resistance number factors in a person's carefulness with spatial distancing, frequency of washing hands, wearing a mask, etc.

A person's resistance in the model is randomly drawn a uniform distribution between from `1` (more likely to be infected) to `1000` (least likely to be infected).

The resistance score is used with two parameters:

- `resistance1`: the threshold below which an infection takes place. By default, this is set to 400. 
- `resistance_penalty`: a value is added to the `resistance1` threshold, increasing the likelihood of infection in the household. The value is drawn from a uniform distribution between 0 and `resistency_penalty` for each household. `resistance_penalty` has a default value of `100`.
- `resistance2`: used with a person's `Age` to increase the liklihood of both active and critical cases among the elderly.





### Observations around transmission locations

> Background: By early April 2020, the COVID-19 pandemic had infected nearly one million people and had spread to nearly all countries worldwide. It is essential to understand where and how SARS-CoV-2 is transmitted. Methods: Case reports were extracted from the local Municipal Health Commissions of 320 prefectural cities (municipalities) in China, not including Hubei province, between 4 January and 11 February 2020. We identified all outbreaks involving three or more cases and reviewed the major characteristics of the enclosed spaces in which the outbreaks were reported and associated indoor environmental issues. Results: Three hundred and eighteen outbreaks with three or more cases were identified, involving 1245 confirmed cases in 120 prefectural cities. We divided the venues in which the outbreaks occurred into six categories: homes, transport, food, entertainment, shopping, and miscellaneous. Among the identified outbreaks, 53.8% involved three cases, 26.4% involved four cases, and only 1.6% involved ten or more cases. **Home outbreaks were the dominant category (254 of 318 outbreaks; 79.9%), followed by transport (108; 34.0%; note that many outbreaks involved more than one venue category).** Most home outbreaks involved three to five cases. We identified only a single outbreak in an outdoor environment, which involved two cases. Conclusions: All identified outbreaks of three or more cases occurred in an indoor environment, which confirms that sharing indoor space is a major SARS-CoV-2 infection risk.

https://www.medrxiv.org/content/10.1101/2020.04.04.20053058v1

## Policies

Supermarkets can be `open` or `closed`. If supermarkets are closed, people do not make any trips to the supermarket.

**NOTE**: can we decrease the number of trips people take to the supermarket? eg limit to one per week under a particular scenario?

Schools can be:

- `open`: on weekdays, all students go to school.
- `only_Year12`: on weekdays, only students in Year 12 go to school
- `closed`: no students go to school. 

**NOTE**: should model opening X days per week, like NSW is doing now.

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

#### COVID-19 status

If a person is infected, their COVID-19 status can remain the same, get plausibly better or plausibly worse:

- an infected asymptomatic person `1` can 
  + `-1` become healed, i.e. this person has remained asymptomatic for the duration of their illness.
  + `1` remain asymptomatic
  + `2` become symptomatic
- an infected symptomatic person `2` can
  + `-1` become healed
  + `2` remain symptomatic
  + `3` become critical
- a critical person `3` can
  + `-2` die
  + `-1` become healed
  + `3` remain critical

To what status the individual moves will depend on the number of days since their infection and conditions drawn from the epidemiology variables.

`do_aus_simulate.cpp` line 571

#### The supermarket

#### Work 

#### School

### Nighttime

At night, each person in `australia` returns to their home.

#### House

