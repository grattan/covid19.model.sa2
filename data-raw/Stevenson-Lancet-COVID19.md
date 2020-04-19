April 18, 2020 

 

 

Dr Sabine Kleinert \
Senior Executive Editor \
The Lancet 

 

 

Dear Sabine 

**RE: The estimated likelihood of eliminating the COVID-19 pandemic in
Australia and New** \
**       Zealand under current public health policy settings: An
Agent-Based-SEIR modelling** \
**        approach** 

As per my email to you earlier this week, please find attached
the abovementioned paper you suggested we
submit under the ***fast track*** process for potential publication in
your journal. 

The paper, led by Jason, is one of the first studies to apply a dynamic
simulation modelling approach to answering important policy
questions namely, what is the prospect of elimination of COVID-19 under
public health responses enacted in Australia and New Zealand, and when
might restrictions imposed by the current responses be halted or
partially removed?  

As you can appreciate, the focus of this paper on an elimination
strategy rather than suppression is extremely important and will be of
considerable value to the global community. Further, we intend providing
the model via a website to all countries who wish to adapt the approach
using their data. 

We look forward to a favourable outcome.  

Keep safe and kind regards 

 

![](media/image1.emf){width="1.6998031496062993in"
height="0.9166666666666666in"}

 

Prof Mark Stevenson 

Prof of Urban Transport and Public Health 

The University of Melbourne

**The estimated likelihood of eliminating the COVID-19 pandemic in
Australia and New Zealand under current public health policy settings:
An Agent-Based-SEIR modelling approach.** 

 

Dr Jason Thompson (PhD)^1^, Prof Mark Stevenson (PhD)^1,2,3^, Prof Rod
McClure (PhD)^4^, Prof Nick Wilson (MBChB)^5^, Prof Michael Baker
(MBChB)^5^, Dr Thiago Herick De Sa (PhD)^6^, Prof Tony Blakely
(PhD)^3^ , Dr Kerry Nice (PhD)^ 1^, Dr Jasper Wijnands (PhD)^ 1^,
Dr Gideon Aschwanden (PhD)^1^, Dr Camilo Cruz  (PhD)^ 1^ 

 

 

1.  Melbourne School of Design, The University of Melbourne, Australia 

<!-- -->

2.  Melbourne School of Engineering, The University of Melbourne,
    > Australia 

<!-- -->

3.  Melbourne School of Population and Global Health, The University of
    > Melbourne,\
    > Australia 

<!-- -->

4.  Faculty of Medicine and Health, The University of New England,
    > Australia 

<!-- -->

5.  Department of Public Health, The University of Otago, New Zealand 

<!-- -->

6.  World Health Organization, Geneva, Switzerland 

 

 

 

 

 

Corresponding author:  \
Dr Jason Thompson \
Transport, Health and Urban Design Research Lab \
Melbourne School of Design \
The University of Melbourne \
Melbourne, VIC 3010 

[[Jason.thompson\@unimelb.edu.au]{.underline}](mailto:Jason.thompson@unimelb.edu.au) 

Phone: +61 0457502134 

**Research in Context** 

 

**Evidence before the study**: An extensive review of data on COVID19
disease transmission was attained from the governments of
Australia and New Zealand along with published reports from China, Italy
and the United Kingdom. Almost all published reports adopted SEIR
age-risk transmission models. Although these models are robust, they are
limited by their inability to assess dynamic interactions between
members of the population during the pandemic; a necessary
attribute when exploring the utility of implemented public health
responses. 

**Added value of the study: **We demonstrate the utility of
applying a agent-based SEIR model to answer previously unanswered
questions namely, what is the prospect of eliminating COVID-19 under
currently implemented public health responses and when can restrictions
imposed by the current strategies be halted or partially removed? This
paper provides important insights that will assist
policymakers with decisions in relation to relaxing currently
implemented physical distancing measures and the likely consequences in
relation to the potential elimination of COVID-19. The authors offer the
model for use and adaptation by other countries wanting to
understand the likely disease progression and policy impact. 

**Implications of all the available evidence: **The findings highlight
that in Australia and New Zealand, Government imposed
shutdowns provide the potential to eliminate the COVID-19
infection within 60 days of initiation, but relaxing these restrictions
risks delayed elimination. This is timely evidence that can influence
the trajectory of the pandemic in these countries. Importantly,
the models developed for this study, can be made available to all
countries and can be readily adapted to
country-specific circumstances thereby providing immediate
insights to governments across a wide range of public health
responses to the pandemic. 

**Abstract** 

**Background: **Countries, globally, are rapidly implementing an array
of public health responses to mitigate the spread of COVID-19. Dependent
on the strategies, there is an opportunity for countries to eliminate
COVID-19. In this study we answer two important questions namely, i)
what is the prospect of elimination of COVID-19 under public health
responses enacted in Australia and New Zealand, and ii)
when might restrictions imposed by the current responses be halted or
partially removed?  

**Methods: **We developed an agent-based SEIR model
(ABM-SEIR) calibrated to current government-reported models
of COVID-19 disease progression in Australia and New Zealand. The
model simulated key aspects of both country's populations, disease, and
social dynamics, as well as the mechanism and effect of public health
policy responses on the mitigation of COVID19. We modelled
disease elimination in both
countries under two policy scenarios namely, compliance
to current physical distancing restrictions and a gradual decay in
compliance to such restrictions. 

**Findings: **If both Australia and New
Zealand maintain current physical distancing restrictions for a 60-day
period (that ceases in late May) there is an
estimated  90% probability that both countries could eliminate COVID-19
from the population by July, 2020. However, if either
country experiences a decay in compliance with these
measures, they could experience a rebound in COVID-19 infections and /
or delayed elimination timeframe.  

**Interpretation: **The findings highlight that in island states such as
Australia and New Zealand, the potential to eliminate the COVID-19
infection in a short-to-medium timeframe is achievable. However,
elimination is contingent on maintaining current restrictions over
a 60-day period and continuing with travel restrictions related to the
closure of country borders to international arrivals. The implications
of elimination are profound as it is likely to deliver opportunities for
countries to respond faster in rebuilding social and economic
infrastructure post-pandemic.  

**Funding: **MS is funded by a NHMRC Fellowship (APP1136250) 

**Introduction** 

The first reported cases of SARS-CoV-2 virus (COVID-19) from Wuhan City
in the Hubei Province, China were reported to the World
Health Organization on December 31, 2019. Since then, the transmission
of COVID-19 has been reported in more than 200 countries with more
than 123,000 COVID-19 related deaths and more than 2.0 million confirmed
infections, globally (1). The impact of this pandemic reaches beyond the
reported health sequelae with significant social, environmental and
economic impacts not seen since the 1918 H1N1 influenza pandemic (2). 

The earliest notification of COVID-19 infections in Australia and New
Zealand were January 26 and February 26, 2020, respectively (3, 4). The
public health response to managing the pandemic in both countries
involves the testing of individuals showing symptoms of COVID-19
infection, isolating individuals (and their close contacts) who test
positive until such time as two consecutive, polymerise chain reaction
(PCR) negative COVID-19 tests, are attained. On a country-scale,
comprehensive physical distancing strategies have been in place since
March 28 in Australia (4) with people remaining at home other than for
essential items and maintaining 1.5 metres between people when in public
whilst in New Zealand, self-isolation was initiated on March 26 (5)
which placed even tighter restrictions on individual movements than in
Australia. Both countries have ongoing enhancements of the health system
capacity, and extensive travel restrictions including the closure of
country boarders to international arrivals. 

The public health response implemented in Australia and New
Zealand initially focussed on mitigation (flattening the epidemic
curve) by instituting the above strategies. But since then, the New
Zealand strategy has explicitly become that of elimination (6).  In
Australia, an explicit strategy remains to be announced, although the
Prime Minister has repeatedly ruled out 'herd immunity'.  The daily
caseloads of COVID-19 in Australia have fallen as rapidly as those in
New Zealand with less stringent lock-down, making a strategy of
elimination (defined as no new cases arising
from local, within country, transmission for 21 days) also possible --
although the Prime Minister is advocating the re-opening of
schools suggesting a likely strategy that
supports  mitigation (2), rather than the elimination.  

The robust modelling implemented by both governments using age and risk
stratified transmission models which incorporate the
susceptible-exposed-infected-recovered (SEIR) concept (7, 8) have
been invaluable in avoiding worst-case scenarios. However, important
questions remain unanswered namely, i) what is the prospect of
elimination of COVID-19 under the current strategies, and ii) when can
restrictions imposed by the current strategies be halted or partially
removed? 

A limitation of the static models used in the government modelling is
the inability to assess ongoing interactions between members of the
population during the pandemic at an individual level (9). For example,
it is important to not only model the influence of social
isolation strategies on reducing the number of contacts individuals are
exposed to, but the behavioural mechanism by which this is achieved. By
modelling the behaviours that lead to disease transmission,
we can better understand policy options for mitigation and importantly,
elimination, and understand the likely patterns of
disease progression among individuals with COVID-19 (10), but also in
the community at large. In this paper, we apply a dynamic agent-based
model that answers these important questions, providing insights about
the future upon which policy decisions can be made.  

**Methods** 

We applied the parameters derived from the static age and risk
stratified transmission models developed for the Australian (7) and New
Zealand (8) governments and integrated them into an agent-based SEIR
model (ABM-SEIR) similar to models described elsewhere (10). The
ABM-SEIR model was scaled to the entire Australian (25 million) and New
Zealand (5 million) populations and it modelled the requisite parameters
to investigate the likelihood of COVID-19 disease progression and the
likelihood of elimination in both countries.  

The ABM-SEIR model has been built using the parameters from the pandemic
in both countries and is capable of taking account of important factors
that influence the COVID-19 infection namely, the key government
strategies now in place, as well as the transmission of the infection
and the time it takes to recover. A unique element of the ABM-SEIR model
used here, is it can simulate the dynamics of COVID-19 at either a
country, state or local level; for the purpose of this paper, we have
modelled the dynamics of COVID-19 at the country-level.  

**Model description** 

For each country, individual 'agents' (represented
as simulated residents of either Australia or New Zealand) were
modelled. Each agent possessed demographic, behavioural, and social
policy response characteristics with their aggregate behaviour,
experiences (e.g., of infection and recovery) and actions used to assess
the effect of COVID-19 across
the populations. Agents moved and interacted in the model, based on
stochastic processes or in response to policies
reflecting government restrictions. In this model, we restricted the
model's use to estimates of COVID-19 disease progression and elimination
scenarios in Australia and New Zealand under two scenarios. Firstly,
under continued public health policy enforcement settings analogous
to those currently in place (and described above), and secondly,
under decaying public compliance with these settings over time.
Specifically, we estimated the dates by which each country is likely
to achieve COVID-19 elimination under current policy settings
with 60% confidence that the disease will be eliminated through to
90% confidence that the disease will be eliminated. 

The computing demands and development time required to build
and run a 1:1 scaled ABM-SEIR model representing millions
of people is considerable making analysis associated with rapid changes
in policies and time-critical decision-making difficult in an
unfolding pandemic. Published models used to inform policy in the early
stages of the pandemic in both the United Kingdom and
Australia were adapted from existing influenza models (2,3). Despite
their influence and utility in the immediacy of the outbreak, they
have also been criticised from the perspective of transparency (e.g.,
the model code is not available), scale (e.g., local vs
national dynamics), and limited incorporation of social and behavioural
dynamics related to compliance that either facilitate
or suppress COVID-19 spread (Squazzioni reference here).  

We applied a multi-scale solution that begins at
a local-scale (n=2500 people), and then scales up to a maximum
representation of 25 million individuals - the Australian population or
5 million (New Zealand's population). Because it is underpinned by
stochastic processes, the ABM-SEIR model was run 100 times to attain the
reported results. We explored four experimental conditions, namely i)
Australia's estimated time to elimination under continuation
and compliance with current policies, ii) Australia's time
to elimination under conditions of decaying compliance, iii) New
Zealand's time to elimination under continuation and compliance with
current policies, and iv) New Zealand's time to elimination under
conditions of decaying compliance. All model iterations ran
for 365 simulated days. All programming, documentation, data and details
related to the calculations, estimations and assumptions are available
for download from the online repository (see [[insert code repository as
citation]{.underline}](https://github.com/JTHooker/COVIDModel)). 

As highlighted above, the model was initially populated by 2500 people
with 2495 people susceptible. At model initialisation, a minimum of 2
people were classified as infected with COVID-19 and could
potentially infect others. In the early stage of the ABM-SEIR model,
agents (simulated people) moved at random headings throughout the
environment. If an infected person encountered a susceptible person,
there was a probability of disease transmission between the infected and
susceptible person. This probability was tuned to produce
early-stage (e.g., day 10
to 30) country-level doubling patterns approximating those reported for
COVID-19 in the literature and approximating an R0 value of
between 2.2 to 2.7 (11,12) while also adjusting for overseas arrivals,
which accounted for 50% of new cases introduced into the
environment. Imported cases began their illness duration in the model in
a marginally advanced state compared with those
who acquired their infection in the community. The mean duration of
illness already elapsed for overseas cases at the time of their
appearance in the model was drawn from a distribution of the incubation
period of existing people. Based on estimates of a/symptomatic case
proportions, 50% of cases were classified as asymptomatic
and demonstrated a transmission likelihood 1/3 that of symptomatic
cases (the remaining 50%). 

Infected people experienced an individually assigned incubation period
drawn from a log-normal distribution with a mean of 5 days
and standard deviation (sd) of 0.44 days. Infected people also
experienced a unique illness period that followed a log-normal
distribution with a mean of 15 days and sd of 0.99 days. If infected,
each person had a likelihood of complying with isolation requests drawn
from a log-normal distribution with a mean of 95%
and sd of 1%. Infected individuals also had a likelihood of death based
on their age-group which was calibrated to local data (Tony's
ref). Deceased people were effectively
'hidden' from interacting with the remaining susceptible, infected and
recovered population. R*t* values were calculated and reported on
an individual basis for each person on the last day of their
infectious period before either recovery or death.  

The timing of public-health and physical distancing restrictions was
calibrated to match that observed in each country. For Australia, Day
0 was estimated as January 16^th^, 10 days prior to the first reported
case. For New Zealand, Day 0 was February 16^th^. Physical
distancing policies were enacted in the model for Australia on day 72
(March 28^th^) and for New Zealand on day 39 (March 26^th^). In
anticipation of the application of restrictions, people began physical
distancing measures 14 days prior to policy implementation up to a
maximum of 85% of people maintaining adequate separation to prevent
transmission of COVID-19, 85% of the time. The increase in physical
distancing behaviours prior to policy implementation followed a
power-law determined by the number of days between the current
day (e.g., t -- 14) and the day of implementation and is consistent with
observed mobility trends in Australia and New Zealand (9). See Table 1
for a summary of the parameters and 'agent' characteristics. The
compliance decay window was implemented in reverse, with the decay in
compliance over time following a power-law determined by the number of
days between the current day (t*~i~*) and 60 days after the introduction
of restrictions in each country (e.g., 60 - t*~i~*). A period of 60 days
was selected in accordance with proposed
policies associated with minimising social, economic and health impacts
of shutdown (10). 

The behaviour of people's in the model in response to the imposition of
physical distancing restrictions was simple; where possible (i.e., 85%
of the time) 85% of people avoided either being located
in, or moving to, locations occupied by other people. The remainder did
not. The selection of people who actively avoided others was updated at
each time-step (i.e., there were no high or low compliance individuals
who always acted pro or anti-socially). Compliance resulted in
a 'spreading-out' of people in the model environment, a reduction in
movement, and a consequent reduction in the ability of people to
pass the virus between one another. Under conditions of shutdown, people
only moved if others who did not observe distancing rules (e.g., through
necessity or non-compliance) moved into the location they were currently
occupying. In such circumstances, compliant agents (people) moved away
by locating the closest currently unoccupied location in their
surrounds. 

To validate the ABM-SEIR model we ran both an unmitigated scenario,
and applied the COVID-19 data from Wuhan, Hubei Province, China. Details
of the validation are reported in the on-line repository
(https://bit.ly/2XI3v3z). Importantly, the reported ABM-SEIR
model developed and reported here, reflects the disease trajectories as
reported in the literature (11,12). 

**Table 1. Parameter estimates and 'Agent' characteristics used in the
ABM-SEIR models**

** ** 

+-----------------------+-----------------------+-----------------------+
| **Parameter           | **Imputed Estimate    | **Imputed Estimate    |
| Estimates **          | (Australia) **        | (New Zealand) **      |
|                       |                       |                       |
|                       |                       | ** **                 |
+=======================+=======================+=======================+
| **Physical            | 85%                   | 85%                   |
| distancing** (% of    |                       |                       |
| people limiting       |                       |                       |
| movement and          |                       |                       |
| maintaining a         |                       |                       |
| distance of 1.5m in   |                       |                       |
| public)** **          |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Physical distancing | 85%                   | 85%                   |
| - time **(% of time   |                       |                       |
| that people           |                       |                       |
| successfully maintain |                       |                       |
| a distance of 1.5m    |                       |                       |
| (Aus) or 2.0m (NZ) in |                       |                       |
| public)**  **         |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Mean incubation     | m = 5, sd = 0.44      | m = 5, sd = 0.44      |
| period **(days,       |                       |                       |
| log-normal)** **      |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Mean illness        | m = 15, sd = 0.99     | m = 15, sd = 0.99     |
| period **(days,       |                       |                       |
| log-normal)** **      |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Mean compliance     | m = 0.95, sd = .01    | m = 0.95, sd = .01    |
| with isolation** (%,  |                       |                       |
| log-normal)** **      |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Super-spreaders as  | 10%                   | 10%                   |
| a proportion of       |                       |                       |
| population**^¥^** **  |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Number of days      | 8                     | 8                     |
| after infection that  |                       |                       |
| new cases are         |                       |                       |
| publicly reported **  |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Mean R0 value       | m = 2.5, sd = 1       | m = 2.5, sd = 1       |
| range **(Days 0 --    |                       |                       |
| 30)** **              |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Day of case 0 (Day  | January 16^th^, 2020  | February              |
| 0)  **                |                       | 16^th^,^ ^2020        |
+-----------------------+-----------------------+-----------------------+
| **Days from case 0 to | 72 (March 28^th^,     | 39 (March 26^th^,     |
| policy enactment **   | 2020)                 | 2020)                 |
+-----------------------+-----------------------+-----------------------+
| **Asymptomatic        | 50%                   | 50%                   |
| Cases **(% of         |                       |                       |
| people)** **          |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Infection rate      | 33%                   | 33%                   |
| of asymptomatic       |                       |                       |
| cases vs symptomatic  |                       |                       |
| cases **              |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Physical distancing | 14 days               | 14 days               |
| anticipation          |                       |                       |
| time-window **        |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Physical distancing | 60 days               |  60 days              |
| compliance decay      |                       |                       |
| window **             |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Case compliance wit | 95%                   | 95%                   |
| h                     |                       |                       |
| quarantine **         |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Target peak Rt      | 2.1 -- 3.0            | 2.1 -- 3.0            |
| across trials **      |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Overseas case impor | 1:1                   | 1:1                   |
| t:                    |                       |                       |
| local case ratio  **  |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Agent               | **Definition **       |                       |
| Characteristics **    |                       |                       |
|                       |                       |                       |
| ** **                 |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Infection           | Infected,             |                       |
| status   **           | susceptible,          |                       |
|                       | recovered, deceased   |                       |
| ** **                 |                       |                       |
+-----------------------+-----------------------+-----------------------+
| **Time now  **        | The number of days    |                       |
|                       | (integer) since an    |                       |
|                       | infected person first |                       |
|                       | became infected with  |                       |
|                       | COVID-19              |                       |
+-----------------------+-----------------------+-----------------------+
| **Age-range **        | The age-bracket       |                       |
|                       | (categorical) of the  |                       |
|                       | person, calibrated to |                       |
|                       | census data deciles   |                       |
|                       | from 0 to 100.        |                       |
+-----------------------+-----------------------+-----------------------+
| **Risk of death **    | The overall risk of   |                       |
|                       | death (float) for     |                       |
|                       | this person based on  |                       |
|                       | their age-profile     |                       |
|                       | having experienced    |                       |
|                       | the disease --        |                       |
|                       | calibrated to         |                       |
|                       | international data    |                       |
+-----------------------+-----------------------+-----------------------+
| **Location **         | The current location  |                       |
|                       | of the simulated      |                       |
|                       | person (agent) in the |                       |
|                       | model interface       |                       |
+-----------------------+-----------------------+-----------------------+
| **Pace  **            | The speed at which    |                       |
|                       | the person moved      |                       |
|                       | around the            |                       |
|                       | environment -- higher |                       |
|                       | speeds resulted in    |                       |
|                       | more close contact    |                       |
|                       | with other people     |                       |
|                       | (agents) in the       |                       |
|                       | model.                |                       |
+-----------------------+-----------------------+-----------------------+
| **Heading **          | The direction of      |                       |
|                       | travel of             |                       |
|                       | the person at the     |                       |
|                       | current time-step. In |                       |
|                       | conjunction with the  |                       |
|                       | scaling approach      |                       |
|                       | described above, the  |                       |
|                       | heading variable was  |                       |
|                       | used to create local  |                       |
|                       | communities and       |                       |
|                       | control interaction   |                       |
|                       | between and across    |                       |
|                       | communities           |                       |
+-----------------------+-----------------------+-----------------------+
| **Contacts **         | A count (integer) of  |                       |
|                       | contacts the person   |                       |
|                       | (agent) had           |                       |
|                       | interacted with in    |                       |
|                       | the past day as they  |                       |
|                       | moved within the      |                       |
|                       | model's environment   |                       |
+-----------------------+-----------------------+-----------------------+

¥ a small proportion of the population transmit infections to far more
people than the majority of the population 

**Findings** 

As at the time of writing (April 15^th^, 2020) there has been a total of
6449 confirmed cases of COVID-19 in Australia, including a total
of 63 deaths, 2698 current infections and 3688 recovered individuals. In
New Zealand, these figures are 1386 cases, 9 deaths, 649 current
infections and 728 recovered individuals. Approximately 50% of reported
cases in Australia and New Zealand originated from outside country
borders, arriving by air and sea.  

Australia: The findings reported from our ABM-SEIR model reflects
the COVID-19 disease experience to April 15th. The estimated disease
pattern progression for Australia differs to New Zealand's in two ways.
Firstly, although elimination is possible within the 60-day window
analogous to current settings, it is comparatively delayed by virtue of
Australia's delayed start to physical distancing restrictions from the
date of identification of Case 0 (72 days), and consequent greater
starting caseload prior to intervention. Nonetheless, the estimates from
the model under consistent public adherence to current policy
settings, suggest there is a 60% likelihood that Australia could
eliminate COVID-19 from the population by May 27^th^, a 70% chance
by June 3^rd^, 80% by June 10^th^, and 90% by June 17^th^  (see Figures
1 and 2). 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\CA209E22.tmp](media/image2.png){width="6.268055555555556in"
height="3.6847222222222222in"} 

**Figure 1.** Estimated Australian disease progression under consistent
compliance\
with physical distancing policies. 

 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\CDE60280.tmp](media/image3.png){width="6.268055555555556in"
height="3.5055555555555555in"} 

**Figure 2**. Estimated likelihood of disease elimination in Australia
under continued public\
adherence to current policy settings. 

Australia appears less likely to eliminate COVID-19 from the
population if compliance to current physical distancing policies decay
over time from a current estimated 85% during the 60-day post
implementation period, assuming a power-law relationship between time
and compliance decay. We estimate there is only a 60% chance that
Australia can eliminate COVID-19 under this scenario (see Figure 4). 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\A1D70C8E.tmp](media/image4.png){width="6.268055555555556in"
height="3.6659722222222224in"} 

**Figure 3. **Estimated Australian disease progression under conditions
of decay in public\
compliance with social distancing policies over 60 days. 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\A0A3B7CC.tmp](media/image5.png){width="6.268055555555556in"
height="3.6506944444444445in"} 

**Figure 4**. Estimated likelihood of disease elimination in Australia
under conditions of public\
compliance decay with social distancing policies over 60 days 

 

New Zealand: Results of the modelling for New Zealand demonstrate a
pattern of disease progression consistent with that observed and
recorded between January 26th and April 15^th^, 2020 (13). Prior to
implementation of the lockdown policy on March 26th, growth in new cases
appeared exponential, but flattened through to April 5th
whereupon they entered a sharp decline. The growth curve was exacerbated
by a significant import of cases from outside New Zealand arriving via
air (5). This pattern is also reflected in our model's results,
providing confidence that the representation of disease transfer between
individuals, case import, and public health interventions such
as physical distancing are adequately represented by the behaviour
of the artificial society.  

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\6ADB0BBA.tmp](media/image6.png){width="6.268055555555556in"
height="3.6659722222222224in"} 

**Figure 5.**  Estimated New Zealand disease progression under
consistent compliance with \
                  physical distancing policies. 

Based on consistent application of and adherence to physical distancing
policies associated with New Zealand's current lockdown strategy, we
estimate there is a 60% chance of elimination (i.e., no new cases and no
current cases) by June 1^st^, an 80% chance by June 7^th^, and a \> 90%
chance by June 21^st^ , 2020 (see Figures 5 and 6). Under conditions of
gradual decay in compliance from 85% to 0% over 60 days, we estimate
that New Zealand can still achieve elimination,
albeit over in marginally delayed estimated period of approximately 2
weeks (see Figure 7). It is of course acknowledged that any new
identified case will effectively delay eradication by an average of 15
days. 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\899AFBD8.tmp](media/image7.png){width="6.268055555555556in"
height="3.658333333333333in"} 

**Figure 6**. Estimated likelihood of disease elimination in New Zealand
under public adherence\
to current policy settings.  

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\92B47A6.tmp](media/image8.png){width="6.268055555555556in"
height="3.6659722222222224in"} 

**Figure 7.** Estimated New Zealand disease progression incorporating
decay\
in behavioural compliance over 60 days from 85% compliance to 0%. 

 

 

![C:\\Users\\stevenson1\\AppData\\Local\\Microsoft\\Windows\\Temporary
Internet
Files\\Content.MSO\\31BB5AA4.tmp](media/image9.png){width="6.268055555555556in"
height="3.658333333333333in"} 

**Figure 8**. Estimated likelihood of disease elimination in New
Zealand under conditions of\
public compliance decay with social distancing policies over 60 days

**Interpretation** 

These findings highlight that for island states such as Australia and
New Zealand, the potential to eliminate the COVID-19 infection in a
short-to-medium timeframe is high. However, elimination is contingent
on maintaining physical distancing policies for a sustained period. The
60-day period modelled in this paper suggests that the sustain period
needs to be until approximately May 26^th^ and May 28^th^  for New
Zealand and Australia, respectively. Furthermore, ongoing travel
restrictions related to the closure of country boarders to international
arrivals would also be needed to ensure elimination is achieved (11). 

Both New Zealand and Australia have similar potential
to attain elimination of COVID-19 by June / July 2020 and hence the
opportunity to return to social and economic activity (at
levels determined by the government). However, our results indicate that
the margin of error for both to achieve elimination is small. Marginal
changes in general non-compliance with physical distancing and avoidance
activities could see COVID-19 persist in the community for an extended
duration. Still, it appears that for both countries the 'go hard, go
early' policy position has expedited reduced transmission rates to this
point. Both countries, however, now find themselves at critical policy
junctures where decisions related to relaxation of measures (prior to
late May) are being considered, with the economic and health policies in
apparent conflict.  

Our finding that island states have the potential to eliminate
COVID-19 is also reflected by the success observed in Taiwan. Taiwan
acted at least 6-8 weeks before both Australia and
New Zealand by imposing border controls on January 25, 2020. An
extensive public health response ensued including, the testing of
individuals showing symptoms of COVID-19
infection (Taiwan has instituted one of the most
comprehensive testing regimes per case of COVID-19) (14), isolating
individuals (and their close contacts), extensive contact tracing and
face masks whilst in public. For the past 10-days, Taiwan has had no new
cases of COVID-19 which if this continues, they will have eliminated
COVID-19 by the end of April. 

The evidence from Taiwan suggests that with early border restrictions
and rigorous public health responses, elimination of COVID-19 is
feasible whilst also maintaining levels of social and economic
activity. Given the delayed border restrictions imposed in Australia and
New Zealand, physical distancing/social distancing strategies for
prolonged periods are needed to achieve the
goal of eliminating COVID-19. With the likelihood of a viable vaccine
some time away, and the prolonged burden on the health system in
pursuing a strategy of 'mitigation' that is, mitigate COVID-19\'s impact
and reduce its incidence, morbidity and
mortality, 'elimination' needs to be the leading public health
response pursued in Australia as it is in Taiwan and now New
Zealand. Importantly, this study provides the timeframe and ongoing
public health response needed to achieve this crucial goal.  

 

 

  

**\
**

**References** 

1.  WHO,
    > 2020. [[https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200411-sitrep-82-covid-19.pdf?sfvrsn=74a5d15\_2]{.underline}](https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200411-sitrep-82-covid-19.pdf?sfvrsn=74a5d15_2) 

<!-- -->

2.  Ferguson NM, Laydon D, Nedjati-Gilani G et al. Impact of
    > non-pharmaceutical intervention (NPIs) to reduce COVID-19
    > mortality and healthcare demand. Imperial College COVID-19
    > Response Team, 16 March
    > 2020. [[https://doi.org/10.25561/77482]{.underline}](https://doi.org/10.25561/77482) 

<!-- -->

3.  Chang, S. L., et al. (2020). \"Modelling transmission and control of
    > the COVID-19 pandemic in Australia.\" [arXiv preprint
    > arXiv:2003.10218]{.underline}. 

<!-- -->

4.  Australian Government Department of Health. New and cumulative
    > COVID-19 cases in Australia by notification date. April 12,
    > 2020. [[https://www.health.gov.au/sites/default/files/documents/2020/04/new-and-cumulative-covid-19-cases-in-australia-by-notification-date\_0.gif]{.underline}](https://www.health.gov.au/sites/default/files/documents/2020/04/new-and-cumulative-covid-19-cases-in-australia-by-notification-date_0.gif) 

<!-- -->

5.  New Zealand Ministry of Health Manatu Hauora. COVID19 Current cases
    > details. [[https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details]{.underline}](https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details) 

<!-- -->

6.  [[https://www.physiciansweekly.com/australia-tightens-social-distancing/]{.underline}](https://www.physiciansweekly.com/australia-tightens-social-distancing/) 

<!-- -->

7.  Baker MG, Kvalsvig A, Verrall AJ, Telfar-Barnard L, Wilson N. New
    > Zealand's elimination strategy for the COVID-19 pandemic and what
    > is required to make it work. *N Z Med J* 2020;133(1512):10-14.  

<!-- -->

8.  Moss R, Wood J, Brown D, Shearer F, Black AJ, Cheng AC, Mc Caw
    > JM, McVernon J. Modelling the impact of COVID-19 in Australia to
    > inform transmission reducing measures and health system
    > preparedness.  

<!-- -->

9.  Google Inc (2020). COVID-19 Community Mobility Report. United
    > States, Google. 

<!-- -->

10. Wilson N. Potential worse case health impacts from COVID-19 pandemic
    > for New Zealand if eradication fails: Report to the New Zealand
    > Ministry of Health. Mar 24, 2020. 

<!-- -->

11. Daly, J. (2020). COVID-19. The endgame and how to get there. [The
    > Grattan Institute]{.underline}. Melbourne,
    > Australia**: **[[https://grattan.edu.au/report/covid-19-the-endgame-and-how-to-get-there/]{.underline}](https://grattan.edu.au/report/covid-19-the-endgame-and-how-to-get-there/). 

<!-- -->

12. Funk S, Bansal S, Bauch CT, Eames KTD, Edmunds J, Galvani
    > AP, Klepac P. Nine challenges in incorporating the dynamics of
    > behaviour in infectious diseases models. Epidemics. 2015.
    > 10:21-25. 

<!-- -->

13. Jansson J. COVID-19 modelling is
    > wrong. [[https://medium.com/\@jamesjansson/covid-19-modelling-is-wrong-f7246e3dc396]{.underline}](https://medium.com/@jamesjansson/covid-19-modelling-is-wrong-f7246e3dc396)  

<!-- -->

14. New Zealand Ministry of Health (2020). COVID-19 - current cases
    > details. Wellington, New Zealand, Government of New Zealand. 

<!-- -->

15. Roser M, Ritchie H, Ortiz-Ospina E, Hasell J. Our World in Data:
    > Coronavirus Disease (COVID-19) Statistics and
    > Research. [[https://ourworldindata.org/coronavirus]{.underline}](https://ourworldindata.org/coronavirus) 

>
