


listings_data<-read_excel("analysis/20240718_Listings/Test Data.xlsx")

SD_summary<-listings_data %>% filter(property_type=="Standalone Dwelling") %>% 
                              group_by(month_stt,region,value_band) %>%
                              summarize(nlistings=sum(listing_count,na.rm=TRUE))


SD_plot<-ggplot() +
         geom_line(data=SD_summary,
                   mapping=aes(x=month_stt, y=nlistings, color=value_band)) +
         facet_wrap(~region) +
         xlab("") +
         ylab("Number of listings") +
         labs(color="Listings Value Bands",
              title="Standalone Dwellings",
              subtitle="Regional Time Series")

SD_plot

SD_500_750<- listings_data %>% filter(property_type=="Standalone Dwelling" & value_band=="02. $500k-$750k") %>% 
             group_by(month_stt,region,bedroom_band) %>%
             summarize(nlistings=sum(listing_count,na.rm=TRUE))


SD_plot_beds<-ggplot() +
   geom_line(data=SD_500_750,
             mapping=aes(x=month_stt, y=nlistings, color=bedroom_band)) +
   facet_wrap(~region) +
   xlab("") +
   ylab("Number of listings") +
   labs(color="Listings Bedroom Bands",
        title="Standalone Dwellings",
        subtitle="$500k-$750k value range")

SD_plot_beds


AP_summary<-listings_data %>% filter(property_type=="Apartment") %>% 
   group_by(month_stt,region,value_band) %>%
   summarize(nlistings=sum(listing_count,na.rm=TRUE))

AP_plot<-ggplot() +
   geom_line(data=AP_summary,
             mapping=aes(x=month_stt, y=nlistings, color=value_band)) +
   facet_wrap(~region) +
   xlab("") +
   ylab("Number of listings") +
   labs(color="Listings Value Bands",
        title="Apartments",
        subtitle="Regional Time Series")

AP_plot

SD_MoM<- SD_summary %>% group_by(month_stt,region) %>% 
         summarise(MonthlyListings=sum(nlistings,na.rm=TRUE)) %>% 
         filter(month_stt>="2020-01-01") %>%  ungroup() %>% 
         arrange(region,month_stt) %>%
         mutate(month_stt=as.Date(month_stt),
                month=lubridate::month(month_stt),
                MoM = case_when(month_stt=="2020-01-01"~ NA,
                                month_stt>="2020-02-01" ~ (MonthlyListings - lag(MonthlyListings)) / lag(MonthlyListings)))

SD_plot_MoM<-ggplot() +
   geom_col(data=SD_MoM,
             mapping=aes(x=month_stt, y=MoM, fill=region),position="dodge") +
   scale_y_continuous(labels = scales::percent) +
   scale_fill_viridis_d() +
   scale_x_date(date_labels="%b-%y",date_breaks  ="1 month") +
   xlab("") +
   ylab("MoM") +
   labs(color="Regions",
        title="Standalone Dwellings",
        subtitle="Month-On-Month variation")

SD_plot_MoM

rc_polygons_tmp<- sf::read_sf("analysis/20240718_Listings/regions_listings/regions_listings.shp") 

rc_polygons<-rc_polygons_tmp %>% mutate(region=case_when(TA2023_V_1=="Otago West Coast"~"Otago Southland",
                                                         TA2023_V_1=="Christchurch City"~"Christchurch",
                                                         TA2023_V_1=="Hamilton City"~"Hamilton",
                                                         TA2023_V_1=="Rotorua District"~"Rotorua",
                                                         TA2023_V_1=="Dunedin City"~"Dunedin",
                                                         TA2023_V_1=="Tauranga City"~"Tauranga",
                                                         TA2023_V_1=="Far North District"~"Far North",
                                                         TRUE~TA2023_V_1))



rc_polygons_sd<-rc_polygons %>% left_join(SD_MoM %>% select(-MoM,-month_stt), by="region",
                                          relationship="many-to-many") %>% filter(!is.na(month)) %>%
                                 mutate(monthname=case_when(month==1~"Jan",
                                                            month==2~"Feb",
                                                            month==3~"Mar",
                                                            month==4~"Apr",
                                                            month==5~"May",
                                                            month==6~"Jun",
                                                            month==7~"Jul",
                                                            month==8~"Aug",
                                                            month==9~"Sep",
                                                            month==10~"Oct",
                                                            month==11~"Nov",
                                                            month==12~"Dec"))

plot_sub_regions_tmp<- rc_polygons_sd %>% filter(month %in% c(3,4,5,6))

neworder<-c("Mar","Apr","May","Jun")
plot_sub_regions<-arrange(transform(plot_sub_regions_tmp,
                  monthname=factor(monthname,levels=neworder)),monthname)

map_rc_listings<-ggplot() +  
   geom_sf(data=plot_sub_regions %>% filter(region=="Auckland" ),fill="black") +
   geom_sf(data=plot_sub_regions %>% filter(region!="Auckland" ),aes(fill = MonthlyListings)) + 
   facet_wrap(~ monthname) + 
   scale_fill_continuous(labels=scales::comma,name = 'Listings',high = "#132B43", low = "#56B1F7") + 
   scale_y_continuous(labels=scales::comma) +
   xlab(expression(paste("Longitude (", degree,"E)"))) +
   ylab(expression(paste("Latitude (", degree,"S)"))) +
   labs(title="Variation on listings numbers over regions during level 4 and level 3",
        subtitle="Excluding Auckland",
        caption="Note: Customised regions differ from regional councils")

map_rc_listings




   