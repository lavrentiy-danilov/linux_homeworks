library(vegan)

library(vegan)
data(mite)
data(mite.env)
data(mite.xy)
head(mite[ , 1:6], 2)

library(ggplot2)
library(gridExtra)
th <- theme(legend.text = element_text(size = 13), legend.title = element_text(size = 14), legend.position = "bottom", text = element_text(size = 10))
th_narrow <- th + theme(legend.key.width = unit(3, "mm"))

theme_set(theme_bw(base_size = 18))
update_geom_defaults("point", list(shape = 19))
p <- ggplot(mite.xy, aes(x = x, y = y)) + geom_point()
p + coord_fixed() + th

#Решение с помощью функции cca()
mite_cca <- cca(mite ~ SubsDens + WatrCont + Substrate + Topo, data = mite.env)

#Проверяем предикторы на мультиколлинеарность
vif.cca(mite_cca)

summary(mite_cca)

#Обилие каких видов сильнее варьирует вдоль оси?
scores(mite_cca, display = "species", choices = 1:5)


#Корреляции между откликами (обилиями видов) и предикторами (средой)
spenvcor(mite_cca)

plot(mite_cca, scaling = "site", main = "scaling 1")

plot(mite_cca, scaling = "species", main = "scaling 2")

#Процедура ССА из пакета vegan
vare.cca <- cca(varespec ~ Baresoil+Humdepth+pH+N+P+K+Ca+Mg+S+Al+Fe, data=varechem)

#Смотреть долго и внимательно. Найти инерцию. Найти собственные числа канонических и неканонических осей. Посмотреть на Proportion explained

summary(vare.cca)

# Сравнить инерцию из summary и расчитанную. Попробовать рассчитать Constrained и Inconstrained инерции. Подсказка ?inertcomp

Inertia<-sum(eigenvals(vare.cca))

# Посмотреть на скоры. Какие выводы можно сделать? 
scores(vare.cca, display = "species", choices = 1:5)

#Ординации в разном масштабе
plot(mite_cca, scaling = "sites", 
     main = "scaling 1, или 'sites' ")

plot(mite_cca, scaling = "species", 
     main = "scaling 2, или 'species'")

# Проверка значимости ординации
anova(mite_cca, permutations = 9999)
anova(mite_cca, by="axis")
anova(mite_cca, by="term")

anova(mite_cca, by="mar")


# Компоненты изменчивости при построении модели с двумя наборами предикторов (возможно только для RDA)
mod <- varpart(mite, ~ SubsDens + WatrCont + Substrate + Topo, ~ x + y, data = cbind(mite.env, mite.xy))

showvarpa




