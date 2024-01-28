### Code to reproduce the results from the paper
# Analysis of Textual Complexity in Danish News Articles on Climate Change


library(tidyverse)
library(tidytext)
library(magrittr)
library(quanteda)
library(quanteda.textstats)
library(GGally)
library(viridisLite)
library(rcartocolor)
library(infer)
library(patchwork)
library(trend)
library(scales)
require(FactoMineR)
require(factoextra)
require(ggblend)

custom_col_readability <-c("#E69F00","#56B4E9","#009E73","#CC79A9")
#custom_period_col<-c('#1b9e77','#d95f02','#7570b3','#e7298a',)


#load the data
# We can not share full-text but only the summarised feature table
infomedia_final_augumented <- read_csv("infomedia_final_augumented.csv")


# find the ** core ** CC articles. (min=2)
infomedia_final_augumented%>%
  filter(WordCount>100)%>%
  filter(!ArticleId%in%non_relevant_articles)%>%
  mutate(BodyText = paste0(Heading,SubHeading,BodyText),
         klima_count = str_count(BodyText,'klimafor*'),
         drivhus_count = str_count(BodyText,'drivhus*'),
         opvarmning_count = str_count(BodyText,'opvarmning'))%>%
  group_by(ArticleId)%>%
  mutate(total = sum(klima_count,drivhus_count,opvarmning_count))%>%
  select(ArticleId,pub_year,klima_count,
         drivhus_count,opvarmning_count,total)%>%
            filter(total>1)->core_cc_articles_df

# get only IDs for filter later 
core_cc_articles_ids<-core_cc_articles_df$ArticleId


# Get an overview of the used data
infomedia_final_augumented%>%
          filter(ArticleId%in%core_cc_articles_ids)%>%
              group_by(Source,pub_medium,news_platforms,pub_year)%>%summarise(count=n())%>%view()


#### randomly sample 
# This part is for analysis in section 3.2.1 Semantic complexity via NMPI 
# Article IDs
set.seed(12345)
core_cc_articles_df%>%
              mutate(period = case_when(pub_year < 2000 ~ 'Period 1 (1990-1999)',
                               pub_year > 1999 & pub_year < 2006 ~ 'Period 2 (2000-2005)',
                               pub_year > 2005 & pub_year < 2011 ~ 'Period 3 (2006-2010)',
                               pub_year > 2010 & pub_year < 2016 ~ 'Period 4 (2011-2015)',
                               pub_year > 20015  ~ 'Period 5 (2016-2021)'))%>%
                group_by(period)%>%sample_n(size=1000)%>%pull(ArticleId)->five_period_PMI_sample



core_cc_articles_df%>%group_by(pub_year)%>%summarise(count = n())%>%view()



# Some helper functions 

# lets calculate some measures
# LIX, TTR, OVIX, Average Sentence Length (ASL),

# NR (needs PoS tags)

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", " ", htmlString))
}

wordLengthCounter6<-function(string){
  word_length_char<-str_length(strsplit(string, " ")[[1]])
  counter<-0
  for (i in word_length_char){
    if(i>6){
      counter<-counter+1
    }
  }
  return(counter)
}


xlwCounter<-function(string){
  word_length_char<-str_length(strsplit(string, " ")[[1]])
  counter<-0
  for (i in word_length_char){
    if(i>14){
      counter<-counter+1
    }
  }
  return(counter)
}



# non relevant articles
non_relevant_articles <-c('e39f8337','e84656f1','e77037a2','e81d2fc2','e893ab64','e6bf352f','Z1028764')
# some of them have a long list of researchers singing an opinion piece etc.


# which aritcles are longer than 100 words? -> delete 


# Let's take the decision and delete sub-headings which otherwise would be
# separate sections/paragraphs 


infomedia_final_augumented%>%
  filter(WordCount>100)%>%
  filter(!ArticleId%in%non_relevant_articles)%>%
  mutate(all_text = paste(Heading,SubHeading,BodyText,sep=' '))%>%
          mutate(all_text = cleanFun(all_text),
                 all_text = str_remove_all(all_text,'Klik her for at åbne originalartiklen på (Jp.dk|Kristeligt-Dagblad.dk|Politiken.dk|Ekstrabladet.dk|Berlingske.dk|Weekendavisen.dk)'),
                 all_text = str_remove_all(all_text,'Klik her'))%>%
                 #all_text = str_remove_all(all_text,'/ritzau/'),
                 #all_text = str_remove_all(all_text,'.+\\..+@[[:alnum:]]+\\..+')
          mutate(word_count = str_count(all_text,'\\w+'),
                 sentence_count = str_count(all_text,'[\\.|:|;|!|?][[:space:]|[:alpha:]]')+1,
                 asl=word_count/sentence_count)%>%rowwise()%>%
          mutate(words_length_6 = wordLengthCounter6(all_text),
                 extra_long_words = xlwCounter(all_text),
                 awl=mean(str_length(strsplit(all_text, " ")[[1]])), 
                 word_types = length(unique(unlist(str_split(all_text,' '))))-1)%>%ungroup%>%
          mutate(LIX = (word_count/sentence_count)+((words_length_6/word_count)*100),
                 OVIX = (log(word_count))/(log(2-(log(word_types)/log(word_count)))))%>%
                  select(-c(Heading,SubHeading,BodyText,all_text))->readability_df




textdiv_calc<-function(text){
  text_tokens<-tokens(text)
  textdiv_vals<-textstat_lexdiv(text_tokens,
                    measure = c('I','MATTR'))
  print(str_length(text))
  print(textdiv_vals)
  return(textdiv_vals)
}


infomedia_final_augumented%>%
  filter(WordCount>100)%>%
  filter(!ArticleId%in%non_relevant_articles)%>%
  mutate(all_text = paste(Heading,SubHeading,BodyText,sep=' '))%>%
  mutate(all_text = cleanFun(all_text),
         all_text = str_remove_all(all_text,'Klik her for at åbne originalartiklen på (Jp.dk|Kristeligt-Dagblad.dk|Politiken.dk|Ekstrabladet.dk|Berlingske.dk|Weekendavisen.dk)'),
         all_text = str_remove_all(all_text,'Klik her'))%>%
            pull(all_text)->all_text_for_map

# Careful ... this takes at least 1hr to run!!!
text_stats_res <- map_df(all_text_for_map,textdiv_calc)
#Bind the rest with the two map results 
complexity_df<-bind_cols(readability_df,text_stats_res)%>%filter(!ArticleId=='Z1028764')
#write to disc


#### here we need to load the results from the syntactic depth and add it 

syntactic_depth <- read_csv("syntactic_depth.csv")%>%select(-1)
syn_depth<-syntactic_depth%>%
      separate(ArticleId,c('article_id','para_id'))%>%
        group_by(article_id)%>%summarise(syn_depth = mean(syntactic_depth))



#### here pos 
pos_tag_count <- read_csv("pos_tag_count.csv")
pos_tags_count%>%
        pivot_wider(names_from = pos_tag,
                        values_from = count,values_fill = 0)%>%
        mutate(nominal_ratio = (NOUN+PROPN+PART+ADP)/(PRON+VERB+ADV))%>%
          select(article_id,nominal_ratio)->nominal_ratio

        


#-------------
# Some descriptive statistics here?

#geom_hex?
my_hex_fun <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_hex(...) +
    scale_fill_viridis_c()
}

complexity_df%>%
  select(asl,awl,LIX,OVIX,I,MATTR,syn_depth,nominal_ratio,avg_pos_share)%>%
      ggpairs(
        lower = list(
          continuous = my_hex_fun
        )
    )+theme_minimal()


##----------------------------------------##
# Here lets do some PCA 
##---------------------------------------##
# Data needs to be scaled mean = 0 SD=1

# PCA to create two dimension of text complexity

pca_res<-PCA(complexity_df%>%filter(ArticleId%in%core_cc_articles_ids)%>%
               column_to_rownames(var="ArticleId")%>%
                  select(asl,awl,LIX,OVIX,I,MATTR,syn_depth), 
                      scale.unit = TRUE, ncp = 2, graph = TRUE)

plot(pca_res)

pca_vars <- get_pca_var(pca_res)

pca_vars$contrib
library("corrplot")
corrplot(pca_vars$contrib, is.corr=FALSE)


# Create Figure 1
fviz_contrib(pca_res, choice = "var", axes = 1, top = 10)+labs(x='Variables')+theme_minimal(base_size = 28)->var_contribute_dim1
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)+labs(x='Variables')+theme_minimal(base_size = 28)->var_contribute_dim2
var_contribute_dim1+var_contribute_dim2+plot_annotation(tag_levels = 'A')


fviz_pca_ind(pca_res)
testi<-bind_cols(pca_res$ind$coord%>%as_tibble(),
           pca_res$ind$coord%>%rownames('ArticleId'))
testi%>%
    left_join(infomedia_final_augumented,
                  by=c('...3'='ArticleId'))%>%
                    rename(ArticleId=`...3`)%>%
                    select(ArticleId,pub_year,
                        pub_medium,news_platforms,Dim.1,Dim.2)->articles_complexity_dims


# Create  Figure 2
articles_complexity_dims%>%
  group_by(pub_year)%>%
  summarise(avg_dim1 = mean(Dim.1))%>%
  ggplot(aes(x=pub_year,y=avg_dim1))+
  geom_point(size=1.5)+
  geom_smooth()+
  #geom_jitter(alpha=0.1,width=0.2)+
  theme_minimal(base_size = 24)+
  geom_hline(yintercept = 0,linetype='dashed',color='black',linewidth=1.1)+
  theme(axis.text = element_text(colour = "black"))+
  #geom_point(data=avg_dim1,aes(x=as.factor(pub_year),y=avg_dim1),color='blue')+
  labs(x='Year',y='Semantic\nComplexity')->sem_complex


articles_complexity_dims%>%
  group_by(pub_year)%>%
  summarise(avg_dim2 = mean(Dim.2))%>%
  ggplot(aes(x=pub_year,y=avg_dim2))+
  geom_point(size=1.5)+
  geom_smooth()+
  #geom_jitter(alpha=0.1,width=0.2)+
  theme_minimal(base_size = 24)+
  geom_hline(yintercept = 0,linetype='dashed',color='black',linewidth=1.1)+
  theme(axis.text = element_text(colour = "black"))+
  #geom_point(data=avg_dim1,aes(x=as.factor(pub_year),y=avg_dim1),color='blue')+
  labs(x='Year',y='Syntactic\nComplexity')->syn_complex


sem_complex+syn_complex+plot_annotation(tag_levels = 'A')


# Man Kendall Trend test
articles_complexity_dims%>%
      group_by(pub_year)%>%summarise(median_dim1=median(Dim.1))%>%
            pull(median_dim1)%>%mk.test()

articles_complexity_dims%>%
  group_by(pub_year)%>%summarise(median_dim2=median(Dim.2))%>%
    pull(median_dim2)%>%mk.test()


articles_complexity_dims%>%
        group_by(pub_year)%>%
          summarise(avg_dim1=mean(Dim.2))%>%view()



# --- ---------- ----
# Here we do timeserie per newspaper type 
# Create Figure 3


articles_complexity_dims%>%
  mutate(news_platforms = ifelse(news_platforms=='public service',
                                 'Public service',news_platforms))%>%
  group_by(pub_year,news_platforms)%>%
  summarise(avg_dim1_per_plat = mean(Dim.1))%>%
  ggplot(aes(x=pub_year,y=avg_dim1_per_plat))+
  geom_point(aes(color=news_platforms),size=1.5)+
  geom_smooth(aes(color=news_platforms))+
  #geom_jitter(alpha=0.1,width=0.2)+
  scale_color_manual(values = custom_col_readability)+
  theme_minimal(base_size = 24)+
  geom_hline(yintercept = 0,linetype='dashed',color='black',linewidth=1.1)+
  theme(axis.text = element_text(colour = "black"))+
  #geom_point(data=avg_dim1,aes(x=as.factor(pub_year),y=avg_dim1),color='blue')+
  labs(x='',subtitle='A) Semantic Complexity',y='')+facet_wrap(vars(news_platforms),ncol = 4)+
  guides(color='none')->sem_type


articles_complexity_dims%>%
  mutate(news_platforms = ifelse(news_platforms=='public service',
                                 'Public service',news_platforms))%>%
  group_by(pub_year,news_platforms)%>%
  summarise(avg_dim2_per_plat = mean(Dim.2))%>%
  ggplot(aes(x=pub_year,y=avg_dim2_per_plat))+
  geom_point(aes(color=news_platforms),size=1.5)+
  geom_smooth(aes(color=news_platforms))+
  #geom_jitter(alpha=0.1,width=0.2)+
  scale_color_manual(values = custom_col_readability)+
  theme_minimal(base_size = 24)+
  geom_hline(yintercept = 0,linetype='dashed',color='black',linewidth=1.2)+
  theme(axis.text = element_text(colour = "black"))+
  #geom_point(data=avg_dim1,aes(x=as.factor(pub_year),y=avg_dim1),color='blue')+
  labs(x='Year',subtitle='B) Syntactic Complexity',y='')+facet_wrap(vars(news_platforms),ncol = 4)+
  guides(color='none')->syn_type

sem_type/syn_type



#----------------------------------------------------------
# Semantic Complexity decreased over the years?
# Let's look at positive pointwise mutual information for three periods 
# Analysis from section 3.2.1
#----------------------------------------------------------

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", " ", htmlString))
}

dansk_stopwords <- read_csv("dansk_stopwords.txt", 
                            col_names = FALSE)


infomedia_final_augumented%>%
  filter(WordCount>100)%>%
  #filter(!ArticleId%in%duplicates_for_removing)%>%
  filter(!ArticleId%in%non_relevant_articles)%>%
  mutate(all_text = paste(Heading,SubHeading,BodyText,sep=' '),
         all_text = cleanFun(all_text),
         all_text = str_remove_all(all_text,'Klik her for at åbne originalartiklen på (Jp.dk|Kristeligt-Dagblad.dk|Politiken.dk|Ekstrabladet.dk|Berlingske.dk|Weekendavisen.dk)'),
         all_text = str_remove_all(all_text,'Klik her'),
         all_text = str_replace_all(all_text,'-','T_T'))%>%
  select(ArticleId,all_text)%>%
  unnest_tokens(input=all_text,output=token,to_lower = 'TRUE')%>%
  unnest_tokens(input=token,output=final_token,token='regex',pattern='\\.')%>%
  mutate(final_token = str_replace_all(final_token,'t_t','-'),
         final_token = str_remove_all(final_token,'^-|-$'))%>%
  filter(!str_detect(final_token,pattern = '^\\d+$'))%>%
  filter(str_detect(final_token,pattern=''))%>%
  filter(str_detect(final_token,pattern='[A-Za-z]'))->infomedia_unigrams

### Here we do the PMI thingy
# Let's only look at words that occur at least 50 times in the corpus 


infomedia_unigrams%>%
  filter(ArticleId%in%five_period_PMI_sample)%>%
  filter(!(final_token %in% dansk_stopwords$X1))%>%
    group_by(final_token)%>%
      mutate(token_count_total = n())%>%
        filter(token_count_total>25)%>%
          left_join(infomedia_final_augumented%>%
                          select(ArticleId,pub_year),
                            by=c('ArticleId'))%>%
          mutate(period = case_when(
                            pub_year < 2000 ~ 'Period1',
                            pub_year > 1999 & pub_year < 2006 ~ 'Period2',
                            pub_year > 2005 & pub_year < 2011 ~ 'Period3',
                            pub_year > 2010 & pub_year < 2016 ~ 'Period4',
                            pub_year > 2015  ~ 'Period5'))%>%
            group_by(final_token,period)%>%
              mutate(token_count_period=n())%>%ungroup()%>%
                select(final_token,token_count_total,
                          period,token_count_period)%>%distinct()%>%
              pivot_wider(names_from = period,
                              values_from = token_count_period,values_fill = 0)%>%
              mutate(pmi_period1 = log((Period1/sum(Period1))/
                                         (token_count_total/sum(token_count_total))),
                     pmi_period2 = log((Period2/sum(Period2))/
                                         (token_count_total/sum(token_count_total))),
                     pmi_period3 = log((Period3/sum(Period3))/
                                         (token_count_total/sum(token_count_total))),
                     pmi_period4 = log((Period4/sum(Period4))/
                                         (token_count_total/sum(token_count_total))),
                     pmi_period5 = log((Period5/sum(Period5))/
                                         (token_count_total/sum(token_count_total))),
                     Npmi_period1 = pmi_period1/-log(Period1/sum(token_count_total)),
                     Npmi_period2 = pmi_period2/-log(Period2/sum(token_count_total)),
                     Npmi_period3 = pmi_period3/-log(Period3/sum(token_count_total)),
                     Npmi_period4 = pmi_period3/-log(Period4/sum(token_count_total)),
                     Npmi_period5 = pmi_period3/-log(Period5/sum(token_count_total)))%>%
                        mutate(across(pmi_period1:pmi_period5,~ifelse(.x<0,0,.x)))->pmi_final


            
pmi_final%>%pivot_longer(Npmi_period1:Npmi_period5,
      names_to = 'period',values_to = 'pmi')%>%
      mutate(period = case_when(
        period=='Npmi_period1' ~ '1990-1999',
        period=='Npmi_period2' ~ '2000-2005',
        period=='Npmi_period3' ~ '2006-2010',
        period=='Npmi_period4' ~ '2011-2015',
        period=='Npmi_period5' ~ '2016-2021'))%>%
ggplot(aes(x=pmi,color=period))+
  #geom_density(lwd=1.2,alpha=0.5)+
  stat_density(geom="line",position="identity",lwd=1.2)+
  scale_color_viridis_d()+labs(x='NPMI',y='Density',color='Period')+
        theme_minimal(base_size = 18)+
          theme(axis.text = element_text(colour = "black"),
                legend.position = 'bottom')


#pmi_final%>%ungroup%>%pivot_longer(pmi_period1:pmi_period5,
#               names_to = 'period',
#               values_to = 'pmi')%>%distinct()%>%select(period,pmi)
# group_by(period)%>%summarise(avg_pmi = mean(period))%>%view()
  

# -------------------------
# Do we see a change in the share of pos-tags over time?  
# -------------------------
# Analysis for section 3.2.2
# Create Figure 5.  
  pos_tag_count <- read_csv("pos_tag_count.csv")
  
  glimpse(pos_tag_count)
  
  pos_tag_count%>%left_join(infomedia_final_augumented%>%
                              select(ArticleId,pub_year),
                            by=c('article_id'='ArticleId'))%>%
    filter(article_id%in%core_cc_articles_ids)%>%
    mutate(period = case_when(
      pub_year < 2000 ~ '1990-1999',
      pub_year > 1999 & pub_year < 2006 ~ '2000-2005',
      pub_year > 2005 & pub_year < 2011 ~ '2006-2010',
      pub_year > 2010 & pub_year < 2016 ~ '2011-2015',
      pub_year > 2015  ~ '2016-2021'))%>%
    group_by(pos_tag,period)%>%
    summarise(tag_count = sum(count))%>%
    group_by(period)%>%
    mutate(total_tag_count = sum(tag_count))%>%ungroup%>%
    mutate(tag_share = (tag_count/total_tag_count))%>%
    filter(!pos_tag%in%c('X','SYM','INTJ'))%>%
    ggplot(aes(x=pos_tag,y=tag_share,fill=period))+geom_col(position = 'dodge2')+
    geom_text(aes(label = round(tag_share,3)*100),
              colour = "black", size = 3.5,
              vjust = -0.5, position = position_dodge(.9))+
    labs(y='Percentage [%]',x='POS Tags',fill='Period')+
    scale_fill_viridis_d()+scale_y_continuous(labels=percent)+
    theme_minimal(base_size = 21)+theme(axis.text = element_text(colour = "black"),
                              legend.position='bottom')
  
  

  
  
      
  


#-------------------------------
# Does the type of newspaper have an effect on syntactic or semantic complexity?
#-------------------------------
# Create Figure 6.

articles_complexity_dims%>%
  ggplot(aes(x=Dim.1,y=Dim.2,color=news_platforms))+
  geom_jitter(width=0.5,alpha=0.7)+
  scale_color_manual(values = custom_col_readability)+
  geom_hline(yintercept = 0,linetype='dashed',color='black')+
  geom_vline(xintercept = 0,linetype='dashed',color='black')+
  labs(x='Semantic Complexity',y='Syntactic Complexity',color='',fill='')+
  theme_minimal(base_size = 21)+theme(legend.position='top',
            axis.text = element_text(colour = "black"))->syn_sem_scatter


# Get a closer look at the sectors   

  articles_complexity_dims%>%
    mutate(complex_cat = case_when(Dim.1 < 0 & Dim.2 < 0 ~ '1.low sem - low syn',
                                   Dim.1 > 0 & Dim.2 < 0 ~ '4.high sem - low syn',
                                   Dim.1 > 0 & Dim.2 > 0 ~ '3.high sem - high syn',
                                   TRUE ~ '2.low sem - high syn'))%>%
    group_by(complex_cat,news_platforms)%>%
    summarise(complex_cat_cout = n())%>%ungroup()%>%
    group_by(news_platforms)%>%
    mutate(news_cat_count=sum(complex_cat_cout))%>%ungroup()%>%
    mutate(complex_cat_share = complex_cat_cout/news_cat_count)%>%
    mutate(complex_cat = factor(complex_cat,levels=c("1.low sem - low syn","2.low sem - high syn","3.high sem - high syn","4.high sem - low syn"))) %>%
    ggplot(aes(x=news_platforms,
               y=complex_cat_share,fill=news_platforms,color=news_platforms))+geom_col(alpha=0.7)+
    geom_text(aes(label = round(complex_cat_share,3)*100),
              colour = "black", size = 4.5,
              vjust = -0.5, position = position_dodge(.9))+
    facet_wrap(vars(complex_cat),ncol=4,labeller = )+
    scale_y_continuous(labels=percent)+
    scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    scale_fill_manual(values=custom_col_readability)+
    scale_color_manual(values=custom_col_readability)+
    labs(x='',y='Percentage [%]')+
    theme_minimal(base_size = 21)+theme(legend.position = 'None',
                                        axis.text = element_text(colour = "black"))->syn_sem_type_bar
  
  
  syn_sem_scatter+syn_sem_type_bar+plot_layout(design="AAA
                                                     AAA
                                                     BBB")
  
  
  
  

  
#----------------------------
# Semantic Complexity - Density and Bootstrap
#---------------------------- 

articles_complexity_dims%>%
          ggplot(aes(x=Dim.1,y=news_platforms,
                     color=news_platforms))+
                        geom_boxplot(outlier.shape = NA)+
                          geom_point(alpha=0.1,position = position_jitter(height = 0.2))+
  scale_color_manual(values = custom_col_readability)+
  theme_minimal()+theme(legend.position = 'bottom')+
      labs(fill='',color='',y="",x='Semantic Complexity')

  articles_complexity_dims%>%
    ggplot(aes(x=Dim.2,y=news_platforms,
               color=news_platforms))+
    geom_boxplot(outlier.shape = NA)+
    geom_point(alpha=0.1,position = position_jitter(height = 0.2))+
    scale_color_manual(values = custom_col_readability)+
    theme_minimal()+theme(legend.position = 'bottom')+
    labs(fill='',color='',y="",x='Syntactic Complexity')
  

articles_complexity_dims%>%
  group_by(news_platforms)%>%
  summarise(median_sem = median(Dim.1),
            median_syn = median(Dim.2),
            avg_sem = mean(Dim.1),
            avg_syn = mean(Dim.2))%>%view()  
  
  

bind_rows(
articles_complexity_dims %>%
  filter(!news_platforms%in%c('Niche papers','public service'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("Quality papers","Tabloid")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Quality papers - Tabloid'))),
articles_complexity_dims %>%
  filter(!news_platforms%in%c('Quality papers','public service'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("Niche papers","Tabloid")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Niche papers - Tabloid'))),
articles_complexity_dims %>%
  filter(!news_platforms%in%c('Quality papers','Niche papers'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("public service","Tabloid")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Tabloid'))),
articles_complexity_dims %>%
  filter(!news_platforms%in%c('Quality papers','Tabloid'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("public service","Niche papers")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Niche papers'))),
articles_complexity_dims %>%
  filter(!news_platforms%in%c('Niche papers','Tabloid'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("public service","Quality papers")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Quality papers'))),
articles_complexity_dims %>%
  filter(!news_platforms%in%c('public service','Tabloid'))%>%
  specify(Dim.1 ~ news_platforms) %>%
  generate(reps = 1000) %>%
  calculate(
    stat = "diff in means",
    order = c("Niche papers","Quality papers")
  )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Niche papers - Quality papers')))
)->sem_bootstrap_compare


#----------------------------
# Syntactic Complexity - Density and Bootstrap
#---------------------------- 

# Create Figure 7.
bind_rows(
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('Niche papers','public service'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("Quality papers","Tabloid")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Quality papers - Tabloid'))),
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('Quality papers','public service'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("Niche papers","Tabloid")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Niche papers - Tabloid'))),
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('Quality papers','Niche papers'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("public service","Tabloid")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Tabloid'))),
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('Quality papers','Tabloid'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("public service","Niche papers")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Niche papers'))),
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('Niche papers','Tabloid'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("public service","Quality papers")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Public Service - Quality papers'))),
  articles_complexity_dims %>%
    filter(!news_platforms%in%c('public service','Tabloid'))%>%
    specify(Dim.2 ~ news_platforms) %>%
    generate(reps = 1000) %>%
    calculate(
      stat = "diff in means",
      order = c("Niche papers","Quality papers")
    )%>%get_ci()%>%bind_cols(as_tibble_col(column_name = 'comparison',c('Niche papers - Quality papers')))
)->syn_bootstrap_compare


sem_bootstrap_compare%>%
  mutate(mid = (lower_ci + upper_ci)/2)%>%
  ggplot(aes(x=mid,xmin=lower_ci,xmax=upper_ci,y=comparison))+
  geom_pointrange(fatten=4.5,linewidth=1.2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black")+
  theme(axis.text = element_text(colour = "black"))+
  theme_minimal(base_size = 16)+
  theme(axis.text = element_text(colour = "black"))+
  labs(x='Semantic Complexity',y='')->sem_bootstrap_compare_plot



syn_bootstrap_compare%>%
  mutate(mid = (lower_ci + upper_ci)/2)%>%
  ggplot(aes(x=mid,xmin=lower_ci,xmax=upper_ci,y=comparison))+
  geom_pointrange(fatten=4.5,linewidth=1.2)+
  geom_vline(xintercept=0, linetype="dashed", color = "black")+
  theme_minimal(base_size = 16)+
  theme(axis.text = element_text(colour = "black"))+
  labs(x='Syntactic Complexity',y='')->syn_bootstrap_compare_plot


sem_bootstrap_compare_plot+syn_bootstrap_compare_plot+plot_annotation(tag_levels = 'A')




complexity_df%>%filter(ArticleId%in%core_cc_articles_ids)%>%write_csv('text_analysis_data.csv')

