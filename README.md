# Лабораторная работа №2

---

**Студент:** Дивеев Даниил Андреевич \
**ИСУ:** 368105 \
**Группа:** P3321 \
**Университет:** НИУ ИТМО \
**Факультет:** Программная инженерия и компьютерная техника \
**Курс:** 3-й курс

---

## Отчет

В лабораторной работе представлена реализация бинарного дерева \
через интерфейс словаря (`bt-dict`).

Требования:

1. Функции:
  - добавление и удаление элементов;
  - фильтрация;
  - отображение (map);
  - свертки (левая и правая);
  - структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования
(как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

### Реализация

```haskell
data BTDict k v
  = Empty
  | Node (BTDict k v) (k, v) (BTDict k v)
  deriving (Show)
```

### Функции

```haskell
-- ДОБАВЛЕНИЕ --
insertBT :: (Ord k) => k -> v -> BTDict k v -> BTDict k v
insertBT k v Empty = Node Empty (k, v) Empty
insertBT k v (Node left (k', v') right)
  | k < k' = Node (insertBT k v left) (k', v') right
  | k > k' = Node left (k', v') (insertBT k v right)
  | otherwise = Node left (k, v) right

-- УДАЛЕНИЕ --
deleteBT :: (Ord k) => k -> BTDict k v -> BTDict k v
deleteBT _ Empty = Empty
deleteBT k (Node left (k', v') right)
  | k < k' = Node (deleteBT k left) (k', v') right
  | k > k' = Node left (k', v') (deleteBT k right)
  | otherwise =
      case (left, right) of
        (Empty, Empty) -> Empty
        (Empty, _) -> right
        (_, Empty) -> left
        _ -> case findMinBT right of
          Just (kMin, vMin) ->
            Node left (kMin, vMin) (deleteBT kMin right)
          Nothing -> left

-- ПОИСК --
lookupBT :: (Ord k) => k -> BTDict k v -> Maybe v
lookupBT _ Empty = Nothing
lookupBT k (Node left (k', v') right)
  | k < k' = lookupBT k left
  | k > k' = lookupBT k right
  | otherwise = Just v'

-- ОТОБРАЖЕНИЕ --
mapValuesBT :: (v -> v2) -> BTDict k v -> BTDict k v2
mapValuesBT _ Empty = Empty
mapValuesBT f (Node left (k', v') right) =
  Node (mapValuesBT f left) (k', f v') (mapValuesBT f right)

-- ФИЛЬТРАЦИЯ --
filterBT :: (Ord k) => ((k, v) -> Bool) -> BTDict k v -> BTDict k v
filterBT _ Empty = Empty
filterBT f (Node left (k', v') right)
  | f (k', v') =
      Node (filterBT f left) (k', v') (filterBT f right)
  | otherwise =
      mergeBT (filterBT f left) (filterBT f right)

-- ЛЕВАЯ СВЕРТКА --
foldlBT :: (a -> (k, v) -> a) -> a -> BTDict k v -> a
foldlBT _ acc Empty = acc
foldlBT f acc (Node left kv right) =
  let accLeft = foldlBT f acc left
      accNode = f accLeft kv
   in foldlBT f accNode right

-- ПРАВАЯ СВЕРТКА --
foldrBT :: ((k, v) -> a -> a) -> a -> BTDict k v -> a
foldrBT _ acc Empty = acc
foldrBT f acc (Node left kv right) =
  let accRight = foldrBT f acc right
      accNode = f kv accRight
   in foldrBT f accNode left
```

### Реализация моноида

 ```haskell
-- ОПЕРАЦИЯ АССОЦИАТИВНОСТИ --
instance (Ord k) => Semigroup (BTDict k v) where
  dict1 <> dict2 = foldlBT (\acc (k, v) -> insertBT k v acc) dict1 dict2

-- НЕЙТИРАЛЬНЫЙ ЭЛЕМЕНТ --
instance (Ord k) => Monoid (BTDict k v) where
  mempty = emptyBT
```


### Дополнительно

Реализованы unit и property тесты:

- [Property](/test/Property/BTDictProps.hs)
- [Unit](/test/Spec/BTDictSpec.hs)

В репозитории настроен CI, который:

- собирает проект
- запускает тесты
- проверяет код через линтер и форматтер

[Workflow](/.github/workflows/haskell.yml)
[Конфиг cabal](/fp-labs.cabal)

### Вывод

В ходе выполнения задач была реализована необходимая структура.
Для структуры реализованы все необходимые функции, структура является
полимофорной, неизменяемой, моноидом.
Исключено протекание API, использован идеоматичный стиль программирования.

Структура и ее функции прошли тестирование, что подтверждает корректность,
