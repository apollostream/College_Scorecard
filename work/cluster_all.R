#source('~/GitHub/College_Scorecard/work/buildStaticDB.R', encoding = 'UTF-8', echo=TRUE)

glmdata_all <- DataSpec$studentBF %>%
  dplyr::select(
    c(-1, -(3:8)), -matches('[^4]_(WHITE|BLACK|ASIAN|OTHER|HISP|NRA|AIAN|UNKN)|2MOR|UNKN|NHPI|AIAN|BF_male|BF_is1st|IND_INC|DEP_INC|(LibraryScience)'),
    -matches('Challenge|_DEP_STAT_|notvet|le24y|OUTOFSTATE|prior|(^BF_[gl][et].+[0-9]+K$)|locale|FarWest|Plains|RockyM|GreatLakes|Southwest|Southeast|NewEngland|MidEast') 
  ) %>% 
  select_if( .predicate = function(x) any(x != x[[1]]) ) %>% 
  filter( complete.cases(.) )
tsne_mat_all <- glmdata_all %>% select(-College) %>% as.matrix() %>% scale()
tsne_all <- Rtsne( tsne_mat_all, perplexity = 10, initial_dims = 12 ) 

mmat <- model.matrix( ~ .:. - 1, as.data.frame(tsne_mat_all))
b <- eigen(cor(mmat))
mmat <- mmat[,apply(b$vectors[,1:200],2,function(x) which.max(abs(x))) %>% unique() %>% sort()]

set.seed( 2393 )
tsne_glmnet_all <- cv.glmnet(
  x      = mmat,
  y      = tsne_all$Y,
  family = 'mgaussian',
  lambda = exp(seq(log(1),log(10),length.out = 50))
)
plot( tsne_glmnet_all )


tsne_glmnet_coef_all <- tsne_glmnet_all %>% coef()
# tsne_glmnet_coef_all$y1[-1] %>%
# { (.)[abs((.)[,1])>0,1] } %>%
# { data_frame(Coefficient = names(.), value = round(.,2)) } %>%
#   print()
# tsne_glmnet_coef_all$y2[-1] %>%
# { (.)[abs((.)[,1])>0,1] } %>%
# { data_frame(Coefficient = names(.), value = round(.,2)) } %>%
#   print()

tsne_coef_df_all <- 
  tsne_glmnet_coef_all$y1 %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  setNames(c("Coefficient","Y1")) %>% 
  full_join(
    tsne_glmnet_coef_all$y2 %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      rownames_to_column() %>% 
      setNames(c("Coefficient","Y2")),
    by = "Coefficient"
  ) %>% 
  filter( abs(Y1) > 1.0E-9 | abs(Y2) > 1.0E-9 ) %>% slice(-1)

tsne_coef_df_all %>% mutate(mag = sqrt(Y1^2+Y2^2)) %>% arrange(desc(mag)) %>% print(n = 30)


# tsne_coef_df %>%
# {
#   ggplot(., aes(x=Y1,y=Y2,label=Coefficient)) +
#     geom_point() +
#     geom_text( check_overlap = TRUE )
# } %>%
#   print()

key_terms <- tsne_coef_df_all %>% 
  mutate(mag= sqrt(Y1^2+Y2^2)) %>% 
  filter(abs(mag)>quantile(abs(mag),0.9)) %>% 
  arrange(desc(mag)) %$% Coefficient %>% setdiff("(Intercept)")


college_names <- glmdata_all %$% 
  College %>%
  { gsub('^[0-9_]+','',. ) } %>%
  { gsub('Northwestern University','NU',.) } %>%
  { gsub('California','Cal',. ) } %>%
  { gsub('Mass.+Inst.+Tech','MIT',. ) } %>%
  { gsub('(Mass|Penn|Wash)[^ ]+ *','\\1',.) } %>%
  { gsub('Polytechnic','Poly',. ) } %>%
  { gsub('Institute of Tech[^ ]+','IT',. ) } %>%
  { gsub('Tech.+Inst.+','Tech',. ) } %>%
  { gsub('State','St',. ) } %>%
  { gsub('University','U',. ) } %>%
  { gsub('(U of )|( U$)','',. ) } %>%
  { gsub('College','Col',. ) } %>%
  { gsub('New York','NY',.)} %>%
  { gsub('International','Intl',.) } %>%
  { gsub('North[^ ]+','N',.)} %>%
  { gsub('South[^ ]+','S',.)} %>%
  { gsub('West[^ ]+','W',.)} %>%
  { gsub('East[^ ]+','E',.)} %>%
  { gsub(' U-','-',.)} %>%
  { gsub('-Penn St ','',.)} %>%
  { gsub(' Col *$','',.)} %>%
  { gsub('-(Main)* Campus','',.)} %>%
  { gsub('^PennSt([^-]+)$','Penn St-\\1',.)} %>%
  { gsub(' and ','&',.)} %>%
  { gsub('Agricultural & Mechanical','A&M',.)}

st_abb <- state.abb %>% setNames( state.name )
for( st_nm in names(st_abb) ){
  college_names %<>% { gsub(st_nm,st_abb[st_nm],.) }
}

categories <- { 
  mmat[,key_terms] %*% 
    (tsne_coef_df_all %>% filter(Coefficient %in% key_terms) %$% Y2) 
} %>%
  sapply(
    function(x,q){ length(q) - sum(x>q) + 1 },
    q=quantile(.,c(0.1,0.25,0.75,0.9))
  ) %>%
  factor()

tsne_df_all <- tsne_all$Y %>% 
  as_tibble() %>% 
  setNames(c("Y1","Y2")) %>% 
  mutate( 
    College = college_names, 
    category = categories,
    Earnings = glmdata_all %$% BF_p_gt110K
  ) %>% 
  dplyr::select( College, category, Earnings, everything() ) %>% 
  mutate_at(funs(1.7*scale(.)),.vars=vars(Y1,Y2))

tsne_df_all %>%
{ 
  ggplot(.,aes(x=Y1,y=Y2,color=category)) + 
    geom_point() + 
    geom_text(aes(label=College),size=3,check_overlap = TRUE) +
    theme( text = element_text( face = 'bold' ) )
} %>%
  print()

f_mult <- max(sqrt(tsne_df_all$Y1^2 + tsne_df_all$Y2^2))/max(sqrt(tsne_coef_df_all$Y1[-1]^2 + tsne_coef_df_all$Y2[-1]^2))
y2_min <- -3.5
tsne_coef_df_all %>% 
  mutate( 
    mag = sqrt(Y1^2 + Y2^2),
    Y2 = pmax(y2_min,Y2*f_mult),
    Y1 = Y1*f_mult,
    Coefficient = gsub('\\([^)]+\\)','',gsub('BF_','',Coefficient))
  ) %>%
  {
    ggplot(., aes( x = Y1, y = Y2 ) ) +
      geom_point( color = 'red', alpha = 0.1 ) +
      geom_text( 
        aes( label = Coefficient), 
        color = 'red',
        alpha = 0.7,
        size = 3, 
        check_overlap = TRUE 
      ) + 
      geom_segment(
        inherit.aes = FALSE,
        data = (.) %>% filter(mag>1),
        aes( x=0, y=0, xend=Y1, yend=Y2 ),
        color = 'red',
        alpha = 0.3,
        arrow = arrow(length = unit(0.03, "npc"))
      ) + 
      geom_text(
        inherit.aes = FALSE,
        data = tsne_df_all,
        aes( x=Y1, y=Y2, label=College ),
        mapping=,
        color = 'black',
        size=3,
        check_overlap = TRUE
      )  +
      geom_point( data=tsne_df_all, aes(x=Y1,y=Y2, size = Earnings ), color='blue',alpha=0.1) +
      ggtitle( "t-SNE Biplot" , subtitle = "(blue = college, red = feature)") +
      theme( text = element_text( face = 'bold' ) ) +
      #scale_y_continuous(limits = c(y2_min,4))
    scale_y_continuous(limits = c(y2_min,4))
  } %>%
  print()



tsne_mat_hc_all <- tsne_df_all %>% select(Y1,Y2) %>% as.matrix() %>% set_rownames(tsne_df_all$College)
hc_all <- hclust( d = dist( tsne_mat_hc_all ), method = 'single' )
n_cluster <- 45
cluster_id_all <- cutree( hc_all, k = n_cluster )

# plot( tsne_mat_hc, pch=20, cex=0.5 )
# for(j in seq_along(cl)){ 
#   points( tsne_mat_hc[ cl[[j]], ], pch=20, col=j, cex=1)
# }

# randomize so adjacent clusters are more likely to have very different colors.
set.seed(137)
cluster_id_all <- setNames( sample.int(n_cluster)[cluster_id_all], names(cluster_id_all) )

tsne_mat_hc_all %>%
  as_tibble() %>%
  mutate( College = names(cluster_id_all), cluster = factor( cluster_id_all ) ) %>%
  {
    ggplot(.,aes( x = Y1, y = Y2, color = cluster ) ) +
      geom_point( size = 1, alpha = 0.3 ) +
      geom_text( aes(label = College ), size = 3, check_overlap = TRUE ) +
      theme(
        text = element_text( face = 'bold' ),
        legend.position = 'none' 
      )
  } %>%
  print()

cluster_id_all <- cutree( hc_all, k = n_cluster )
y2_min <- -4
tsne_coef_df_all %>% 
  mutate( 
    mag = sqrt(Y1^2 + Y2^2) ,
    Y2 = pmax(y2_min,Y2),
    Coefficient = gsub('\\([^)]+\\)','',gsub('BF_','',Coefficient))
  ) %>%
  {
    ggplot(., aes( x = Y1, y = Y2 ) ) +
      geom_point( color = 'red', alpha = 0.1 ) + 
      geom_segment(
        inherit.aes = FALSE,
        data = (.) %>% filter(mag>1),
        aes( x=0, y=0, xend=Y1, yend=Y2 ),
        color = 'red',
        alpha = 0.3,
        arrow = arrow(length = unit(0.03, "npc"))
      ) + 
      geom_text(
        inherit.aes = FALSE,
        data = tsne_mat_hc_all %>%
          as_tibble() %>%
          mutate( 
            College = names(cluster_id_all), 
            cluster = factor( (cluster_id_all %% 7) + 1 )
          ),
        aes( x=Y1, y=Y2, label=College, color = cluster ),
        mapping=,
        show.legend = FALSE,
        size=2,
        check_overlap = TRUE
      ) +
      geom_text( 
        aes( label = Coefficient ), 
        color = 'black',
        size = 3, 
        check_overlap = TRUE
      )  +
      geom_point( 
        data = tsne_mat_hc_all %>%
          as_tibble() %>%
          mutate( 
            College = names(cluster_id_all), 
            cluster = factor( (cluster_id_all %% 7) + 1 ),
            cluster_shape = factor( (cluster_id_all %% 6) + 1 )
          ), 
        aes(x=Y1,y=Y2, color = cluster, shape = cluster_shape ), 
        show.legend = FALSE,
        alpha=0.3
      ) +
      ggtitle( "t-SNE Biplot" ) +
      theme( text = element_text( face = 'bold' ) ) +
      scale_y_continuous(limits = c(y2_min,5))
  } %>%
  print()

pred <- tsne_glmnet_all %>% predict( newx = mmat ) %>% drop()
plot(pred[,1],tsne_all$Y[,1])
plot(pred[,2],tsne_all$Y[,2])
