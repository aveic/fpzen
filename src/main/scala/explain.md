СЕЙЧАС:
=======

В основном проекте своя таблица:
public.tbl_order: (тут 16 млн заказов)
  id | status
   1 | deliverd
   2 | intransit



Так же доступна таблица (сюда СЕЙЧАС пишет только трекинг, но читает основной проект):

tracking_stale.tbl_order_track_information (тут 10 млн заказов)
  order_id (UNIQUE) | money_status
                  2 | paid
                  1 | unpaid

В основном проекте куча подобных SELECTы:

SELECT * FROM tbl_order
INNER JOIN tbl_order_track_information AS oti ON oti.order_id = o.id
WHERE o. --- bla bla bla

Это проблема, т.к. в tbl_order_track_information UPDATE идут 300 в секунду. Апдейту могут идти по всем 10 млн заказов.


Как хотим:
==========
Таблицы те же, но в tracking.tbl_order_track_information не пишет онлайн трекинг. Т.е. отсюда читает только основной проект.

Как примерно достичь:

Есть допустим схема

tracking_online:
 в ней tbl_order_track_information с такой же структурой. Но без индексом. И сюда пишет только трекингов. Отсюда основной проект не читает.


Раз в N минут надо делать каким-то образом копию, так
чтобы содержимоe  tracking_online.tbl_order_track_information целиком переходило в tracking_stale.tbl_order_track_information
И основной проект начинает видеть новые данные прозрачно. Без обрыва текущих запросов
При этом никуда нельзя девать tracking_online.tbl_order_track_information, т.к. туда пишет постоянно трекинг.



Почему не работают партиции?
============================

Допустим есть таблица tracking_online.tbl_order_track_information?

Допустим есть партиции (допустим по check(id range))

допустим есть партиция:
tracking_online.tbl_order_track_information_1 (CHECK ID BETWEN 1 AND 1000)
Что она даёт.
Сюда будет писать трекинг и отсюда же будет читать проект.

Т.е. это то, что есть сейчас, только в 100 раз хуже, т.к. партиции в pg сейчас ужасно работают.


у нас уникальные ID заказов. Данные не append-only.

























