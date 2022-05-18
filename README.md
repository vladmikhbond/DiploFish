# DiploFish

Файлы
templateF.txt - шаблон документа
dataF.txt - данные к шаблону
Фамилия.txt - личные данные

Шаблон содержит [личные ключи] и [[общие ключи]].
Общие ключи заменяются значениями из файла dataF.txt. Выбирается одно из альтернативных значений.
Личные ключи заменяются значениями из файла Фамилия.txt. Здесь соотношение 1:1. 

Алгоритм.
1.Все загружаем в память. Имя файла личных данных - в ком строке.
2.В шаблоне документа заменяем каждый общий ключ одним из возможных значений. 
  Заметим, что в значениях также могут быть ключи, общие и личные.
3.Заменяем каждый личный ключ значением.
4.Буквы после точки делаем заглавными.