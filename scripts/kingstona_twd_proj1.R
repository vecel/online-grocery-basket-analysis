aisles <- read.csv("archive/aisles.csv")
departments <- read.csv("archive/departments.csv")
order_products_prior <- read.csv("archive/order_products__prior.csv")
order_products_train <- read.csv("archive/order_products__train.csv")
orders <- read.csv("archive/orders.csv")
products <- read.csv("archive/products.csv")
zam_num <- order_products_prior %>% 
  left_join(products, by='product_id')
 zam_num <- zam_num %>% select(order_id,aisle_id)

zam_num %>% 
  group_by(order_id) %>% 
  summarise(PSY=(aisle_id==40), KOTY=(aisle_id==41)) %>% 
  ungroup() %>% 
  filter(PSY & !KOTY) -> tak_nie
x <- tak_nie %>% pull(order_id)
zam_num %>% filter(order_id %in% x) -> psiezam
psiezam %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> psiealko
zam_num %>% 
  group_by(order_id) %>% 
  summarise(PSY=(aisle_id==40), KOTY=(aisle_id==41)) %>% 
  ungroup() %>% 
  filter(!PSY & KOTY) -> nie_tak
x <- nie_tak %>% pull(order_id)
zam_num %>% filter(order_id %in% x) -> kociezam
kociezam %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> kociealko
zam_num %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> alko
psy <- length(unique(psiezam %>% pull(order_id)))
psyal <- length(unique(psiealko %>% pull(order_id)))
koty <- length(unique(kociezam %>% pull(order_id)))
kotyal <- length(unique(kociealko %>% pull(order_id)))
ordery <- length(unique(zam_num %>% pull(order_id)))
orderyal <- length(unique(alko %>% pull(order_id)))
ogolli <- as.data.frame(zam_num %>% pull(order_id) %>% table)
ogolli <- ogolli %>% rename(order_id=".")
psyli <- as.data.frame(psiezam %>% pull(order_id) %>% table)
psyli <- psyli %>% rename(order_id=".")
kotyli <- as.data.frame(kociezam %>% pull(order_id) %>% table)
kotyli <- kotyli %>% rename(order_id=".")
mniejszy %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> alkomniejsordery <- length(unique(zam_num %>% pull(order_id)))
unimniej <- length(unique(mniejszy %>% pull(order_id)))
unialkomniej <- length(unique(alkomniejszy %>% pull(order_id)))
wektor_prawdy <- c()
for (i in 1:15)
{
  coto %>% 
    filter(number_of_products<=i) %>% 
    pull(order_id) -> filtr
  zam_num %>% 
    filter(order_id %in% filtr) -> ponadi
    ponadi %>% 
    group_by(order_id) %>% 
    summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
    ungroup() %>% 
    filter(ds | do | sd | sdc| stc) %>% 
    pull(order_id) -> alkordery_nieunikalne
    length(unique(alkordery_nieunikalne)) -> alkol
    length(unique(ponadi$order_id)) -> wszystkol
    wektor_prawdy <- c(wektor_prawdy, alkol/wszystkol)
}
ramka_prawdy %>% 
  ggplot(mapping=aes(x=licz, y=wektor_prawdy-0.023)) + geom_bar(stat='identity') -> plot
plot + ggtitle('Udział zamówień z alkoholem w ogólnej liczbie zamówień w zależności od liczby prouktów')+xlab("Liczba produktów w zamówieniu")+ylab('Udział zamówień z alkoholem - 0,023')
kociatabela <- as.data.frame(table(kociezam %>% pull(order_id))) %>% rename(order_id="Var1")
psiatabela <- as.data.frame(table(psiezam %>% pull(order_id))) %>% rename(order_id="Var1")
tabela <- as.data.frame(table(zam_num %>% pull(order_id))) %>% rename(order_id="Var1")
srkota <- mean(kociatabela %>% pull(Freq))
srpsa <- mean(psiatabela %>% pull(Freq))
sr <- mean(tabela %>% pull(Freq))
wier <- c("psy", "koty", "całość")
war <- c(srkota, srpsa, sr)
war<- round(war,digits=2)
ram <- cbind(wier,war)
ram <- as.data.frame(ram)
ram %>% ggplot(mapping=aes(x=wier, y=war))+geom_bar(stat='identity') + ggtitle("Średnia liczba produktów w zamówieniu w zależności od koszyka") + xlab("Koszyk") + ylab("Średnia") 
war <- c(0.063, 0.071, 0.026)
ram %>% ggplot(mapping=aes(x=wier, y=war))+geom_bar(stat='identity') + ggtitle("Udział zamówień z alkoholem w ogólnej liczbie zamówień") + xlab("Koszyk") + ylab("Udział") 

