%===========================
% Atakan Kurt - 200104004044
% HW4: Part-1
%===========================

% Delivery personnel:
% o A unique ID.
% o Capacity to carry (in kg).
% o Work hours (in 4-hour increments including the entire day).
% o Current delivery job (if any).
% o Current location (one of the places as described below).
deliveryPersonnel(dp1, 10, 24, none, instituteX).
deliveryPersonnel(dp2, 20, 12, none, library).
deliveryPersonnel(dp3, 50, 16, object3, cafeteria).

% Places:
% o A unique ID.
place(adminOffice).
place(cafeteria).
place(engineeringBuilding).
place(library).
place(socialSciencesBuilding).
place(lectureHallA).
place(instituteY).
place(instituteX).

% A set of objects to be delivered:
% o A unique ID.
% o Weight in kg.
% o Pick up place (as described above).
% o Drop of place (as described above).
% o Urgency of the delivery (low, medium, high).
% o ID of the delivery person if in transit.
object(object1, 5, cafeteria, adminOffice, medium, none).
object(object2, 8, socialSciencesBuilding, instituteX, high, none).
object(object3, 4, lectureHallA, instituteY, low, dp3).
object(object4, 6, library, cafeteria, high, none).
object(object5, 7, socialSciencesBuilding, engineeringBuilding, medium, none).

% Routes that shows shortest path from place1 to place2.
% Institute X
route(instituteX, instituteX, 0).
route(instituteX, adminOffice, 11).
route(instituteX, cafeteria, 10).
route(instituteX, engineeringBuilding, 14).
route(instituteX, library, 10).
route(instituteX, socialSciencesBuilding, 8).
route(instituteX, lectureHallA, 16).
route(instituteX, instituteY, 13).

% Social Sciences Building
route(socialSciencesBuilding, library, 2).
route(socialSciencesBuilding, cafeteria, 2).
route(socialSciencesBuilding, instituteX, 8).
route(socialSciencesBuilding, socialSciencesBuilding, 0).
route(socialSciencesBuilding, instituteY, 5).
route(socialSciencesBuilding, lectureHallA, 8).
route(socialSciencesBuilding, engineeringBuilding, 6).
route(socialSciencesBuilding, adminOffice, 3).

% Library
route(library, cafeteria, 5).
route(library, adminOffice, 1).
route(library, engineeringBuilding, 5).
route(library, instituteY, 3).
route(library, socialSciencesBuilding, 2).
route(library, library, 0).
route(library, lectureHallA, 6).
route(library, instituteX, 10).

% Cafeteria
route(cafeteria, library, 5).
route(cafeteria, socialSciencesBuilding, 2).
route(cafeteria, adminOffice, 4).
route(cafeteria, cafeteria, 0).
route(cafeteria, lectureHallA, 9).
route(cafeteria, instituteX, 10).
route(cafeteria, engineeringBuilding, 7).
route(cafeteria, instituteY, 7).

% Admin Office
route(adminOffice, cafeteria, 4).
route(adminOffice, library, 1).
route(adminOffice, engineeringBuilding, 3).
route(adminOffice, instituteY, 4).
route(adminOffice, instituteX, 11).
route(adminOffice, lectureHallA, 5).
route(adminOffice, adminOffice, 0).
route(adminOffice, socialSciencesBuilding, 3).

% Engineering Building
route(engineeringBuilding, adminOffice, 3).
route(engineeringBuilding, library, 5).
route(engineeringBuilding, lectureHallA, 2).
route(engineeringBuilding, engineeringBuilding, 0).
route(engineeringBuilding, socialSciencesBuilding, 6).
route(engineeringBuilding, instituteX, 14).
route(engineeringBuilding, instituteY, 5).
route(engineeringBuilding, cafeteria, 7).

% Lecture Hall A
route(lectureHallA, engineeringBuilding, 2).
route(lectureHallA, instituteY, 3).
route(lectureHallA, lectureHallA, 0).
route(lectureHallA, instituteX, 16).
route(lectureHallA, library, 6).
route(lectureHallA, adminOffice, 5).
route(lectureHallA, cafeteria, 9).
route(lectureHallA, socialSciencesBuilding, 8).

% Institute Y
route(instituteY, lectureHallA, 3).
route(instituteY, library, 3).
route(instituteY, instituteY, 0).
route(instituteY, socialSciencesBuilding, 5).
route(instituteY, cafeteria, 7).
route(instituteY, adminOffice, 4).
route(instituteY, instituteX, 13).
route(instituteY, engineeringBuilding, 5).

% Rule for if a deliveryPersonnel can deliver the given object or not in given time.
deliveryPersonnelCanDeliver(DeliveryPersonnelId, ObjectId, TimeGiven) :-
    deliveryPersonnel(DeliveryPersonnelId, Capacity, WorkHours, none, CurrentLocation),
    object(ObjectId, Weight, PickUp, DropOff, _, none),
    Weight =< Capacity,
    route(CurrentLocation, PickUp, Time1),
    route(PickUp, DropOff, Time2),
    TotalDeliverTime is Time1 + Time2,
    TotalDeliverTime =< WorkHours,
    TotalDeliverTime =< TimeGiven.

% Query for if given object is in transit or not.
query(ObjectId, _) :-
    object(ObjectId, _, _, _, _, Var),
    Var \= none,
    write('In transit by '), write(Var), nl, !.

% Query for if given object can not be delivered by anyone.
query(ObjectId, TimeGiven) :-
    object(ObjectId, _, _, _, _, none),
    findall(DeliveryPersonnelId, deliveryPersonnelCanDeliver(DeliveryPersonnelId, ObjectId, TimeGiven), DeliveryPersonnelList),
    DeliveryPersonnelList = [],
    write('Object '), write(ObjectId), write(' cannot be delivered by anyone.'), nl, !.

% Query for listing all deliveryPersonnel that can deliver given object in given time.
query(ObjectId, TimeGiven) :-
    object(ObjectId, _, _, _, _, none),
    findall(DeliveryPersonnelId, deliveryPersonnelCanDeliver(DeliveryPersonnelId, ObjectId, TimeGiven), DeliveryPersonnelList),
    DeliveryPersonnelList \= [],
    write('Object '), write(ObjectId), write(' can be delivered by: '), nl,
    printDeliveryPersonnelListWithTime(DeliveryPersonnelList, ObjectId, TimeGiven).

% Helper rule for printing the list of deliveryPersonnel with delivery time.
printDeliveryPersonnelListWithTime([], _, _).
printDeliveryPersonnelListWithTime([DeliveryPersonnelId|Rest], ObjectId, TimeGiven) :-
    write('Delivery Personnel ID: '), write(DeliveryPersonnelId),
    deliveryPersonnelCanDeliver(DeliveryPersonnelId, ObjectId, TimeGiven),
    write(' - In Total Delivery Time: '), 
    deliveryPersonnel(DeliveryPersonnelId, _, _, none, CurrentLocation),
    object(ObjectId, _, PickUp, DropOff, _, none),
    route(CurrentLocation, PickUp, Time1),
    route(PickUp, DropOff, Time2),
    TotalDeliverTime is Time1 + Time2,
    write(TotalDeliverTime), write(' hours'), nl, !,
    printDeliveryPersonnelListWithTime(Rest, ObjectId, TimeGiven).