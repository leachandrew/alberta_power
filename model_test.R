test_3<-
  test_2 %>%
  mutate(offer_gen="All")%>%
  filter(size>0)%>%  #don't include zero-sized blocks - this helps section out issues with zero wind and solar hours too.
  arrange(date,he,Plant_Type,price) %>%
  group_by(date,he,Plant_Type,offer_gen)%>% 
  mutate(merit_type=cumsum(size)/sum(size),
         merit_co2=cumsum(co2_est*size/1000), #cumulative tonnes of emissions across the merit order
         merit_ctax=(ctax_cost), #marginal compliance costs, $ per mwh
         merit_oba=(oba_val),#marginal oba value, $ per mwh
         #merit_net_comp=(compliance_cost)
  )%>%
  summarize(
    #place offer percentiles and prices in lists of vectors
    offers=list(offer_store),plants=list(plant_store),
    total_offers=sum(size),available_mw=sum(available_mw),dispatched_mw=sum(dispatched_mw),renew_gen=sum(renew_gen,na.rm = T),
    merit=list(merit_type*100),price=list(price),co2_est=list(merit_co2),ctax_cost=list(merit_ctax),oba_val=list(merit_oba)
  )



library(modelsummary)

# Suppose you have a model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Export to LaTeX table
modelsummary(model, output = "latex")