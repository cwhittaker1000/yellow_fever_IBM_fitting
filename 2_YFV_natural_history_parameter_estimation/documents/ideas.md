# things worth considering
- could fit directly to estimated time of death: the reason being that potentially
- the posterior predictive actual death series is very sharp (explaining the high R0s)
- could still fit to observed deaths and check vs time-to-death: just eyeballing
- the series, the time from death to observations seems far too high vs estimated time of
- death: this is a little tricky to do though since we'll still need a probability of
- detection or would it just mean simply switching from D_obs -> D?
- may want to be more realistic in terms of date when all dead observed
- just fit to whole series? This most likely to lead to more reasonable estimates



# Plan for ABC simulations

* Fit to "time_since_death" data using (say) ML or perhaps do something easier since the data here are quite poor -> use to make prior for p_obs
* Pick a "best" set of priors and run sims with those
* Do a sensitivity analysis around the best priors



# General notes

* observation_time PPCs: not sure how useful these are: i) 0.5 days is probably the least time an observer would say and ii) lots of corpses weren't observed so the max in the data is likely a massive understatement. This is probably why the p values are near 1
* SEI model probably way more appropriate since once mosquitoes are infected they then, disregarding even if there is a latency period, mosquitoes need to bite them and then bite another -- so there will definitely be a lag period:
  * Hindle (1930) seems to suggest that intrinsic incubation period may be quite short (<day)
  * Johansson (2010) uses sources on the extrinsic incubation period for both human and monkey transmission and finds an eip of 10 days
* Need to correct initial states: think I've one too many monkeys at the moment given 5 died before the start of the period