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



### Workplaces

### Schools

### ACARA schools data: `schools.fst`

School locations (`school-locations.xlsx`) and profiles (`school-profile.xlsx`)
sourced from ACARA's Data Access Program: https://www.acara.edu.au/contact-us/acara-data-access.




#### SA2 home and Destination Zone work: `data-raw/int/hw_sa2_dzn.fst`

Source: '2016 Census - Counting Employed Persons, Place of Work (POW)'

Variables:

- `SA2 (UR)`
- `DZN (POW)`



#### Dwelling type by SA1: `nonprivate_sa1.fst`

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


<!-------------------------------------------------------------------------------->
## Epidemiological input

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


### Observations around transmission locations

> Background: By early April 2020, the COVID-19 pandemic had infected nearly one million people and had spread to nearly all countries worldwide. It is essential to understand where and how SARS-CoV-2 is transmitted. Methods: Case reports were extracted from the local Municipal Health Commissions of 320 prefectural cities (municipalities) in China, not including Hubei province, between 4 January and 11 February 2020. We identified all outbreaks involving three or more cases and reviewed the major characteristics of the enclosed spaces in which the outbreaks were reported and associated indoor environmental issues. Results: Three hundred and eighteen outbreaks with three or more cases were identified, involving 1245 confirmed cases in 120 prefectural cities. We divided the venues in which the outbreaks occurred into six categories: homes, transport, food, entertainment, shopping, and miscellaneous. Among the identified outbreaks, 53.8% involved three cases, 26.4% involved four cases, and only 1.6% involved ten or more cases. **Home outbreaks were the dominant category (254 of 318 outbreaks; 79.9%), followed by transport (108; 34.0%; note that many outbreaks involved more than one venue category).** Most home outbreaks involved three to five cases. We identified only a single outbreak in an outdoor environment, which involved two cases. Conclusions: All identified outbreaks of three or more cases occurred in an indoor environment, which confirms that sharing indoor space is a major SARS-CoV-2 infection risk.

https://www.medrxiv.org/content/10.1101/2020.04.04.20053058v1

### Epidemiological inputs

From Imperial College London, R_0 under certain intervention:

* see covid19model repo and my results
  - R0 [0.7, 1.3] post lockdown (e.g. Germany)
  - pR0 [0, 0.3] social distancing (0 no effect, 1 elimination)
  - pR0 [0.5, 0.8] lockdown (i.e. only essential excursions permitted)




<!-------------------------------------------------------------------------------->
## Simulation

The `australia` dataset is loaded. 