# covid19.model.sa2

A model of the COVID-19 pandemic stratified by SA2.

### Epidemiological inputs

From Imperial College London, R_0 under certain intervention:

* see covid19model repo and my results
  - R0 [0.7, 1.3] post lockdown (e.g. Germany)
  - pR0 [0, 0.3] social distancing (0 no effect, 1 elimination)
  - pR0 [0.5, 0.8] lockdown (i.e. only essential excursions permitted)





### ABS Census data

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



#### SA2 home and SA2 work: `data-raw/int/hw_sa2_sa2.fst`

Source: '2016 Census - Counting Employed Persons, Place of Work (POW)'

Variables:

- `SA2 (UR)`
- `SA2 (POW)`



#### SA2 home and SA2 work: `data-raw/int/hw_sa2_dzn.fst`

Source: '2016 Census - Counting Employed Persons, Place of Work (POW)'

Variables:

- `SA2 (UR)`
- `DZN (POW)`


### ACARA schools data: `schools.fst`

School locations (`school-locations.xlsx`) and profiles (`school-profile.xlsx`)
sourced from ACARA's Data Access Program: https://www.acara.edu.au/contact-us/acara-data-access.
