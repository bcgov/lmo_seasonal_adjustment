#compare to adjusted data that was provided------------
mystery_adjusted <- readxl::read_excel(here::here("raw_data", rd_files[endsWith(rd_files, "Seasadj.xlsx")]))%>%
  mutate(date = tsibble::yearmonth(date))%>%
  pivot_longer(cols = -date, names_to = "name", values_to = "mystery_adjustment")%>%
  mutate(name = str_sub(name, end = -5))
#join STL adjusted and mystery_adjusted--------
both <- full_join(adjusted, mystery_adjusted)
#plot of Robust STL deseasonalized data vs.  Mystery deseasonalized-----------
plt <- ggplot(both, aes(season_adjust, mystery_adjustment, colour = name))+
  geom_point(alpha = .25)+
  geom_abline(slope = 1, intercept = 0)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  scale_colour_viridis_d()+
  labs(title = paste("Correlation between R seasonal adjustment and Mystery Adjustment: ",
                     round(cor(both$season_adjust, both$mystery_adjustment), 5)))
plt <- aest::aest_fix_labs(plt)
print(plotly::ggplotly(plt))



