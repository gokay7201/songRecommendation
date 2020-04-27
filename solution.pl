% mehmet gokay yildiz
% 2017400072
% compiling: yes
% complete: yes

features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).

filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).

%getArtistTracks(+ArtistName, -TrackIds, -TrackNames).
 getArtistTracks(ArtistName, TrackIds, TrackNames) :- artist(ArtistName,_,Ho),
    getAlbumes(Pro1, Ho,0), getAlbumes(Pro2,Ho,1),
    flatten(Pro1,TrackIds), flatten(Pro2, TrackNames).
%iterating album list
getAlbumes([],[],_).
getAlbumes(List,[H|T],X):- album(H,_,_,L),
    getTracks(Lisk,L,X),
    getAlbumes(Bebe,T,X),
    List = [Lisk|Bebe].
%iterating track list of an album
getTracks([],[],_).
getTracks(Lisp, [H|T],X):-
    trackData(Y,H,X),
    getTracks(Listem, T,X),
    Lisp = [Y|Listem].
trackData(H,H,0).
trackData(Y,H,1):-track(H,Y,_,_,_).  

%albumFeatures(+AlbumId, -AlbumFeatures).
albumFeatures(AlbumId,AlbumFeatures):-
    album(AlbumId,_,_,T),trackLoop(Hello, T,X), divideLoop(Hello,X,AlbumFeatures).
% to sum up all of features of the tracks of an album and counting them 
trackLoop(Filter,[H|[]],1):-  track(H,_,_,_,Ved), filter_features(Ved,Filter).
trackLoop(Result,[Hem|T],Y):- 
    track(Hem,_,_,_,Ded),
    filter_features(Ded,Filtered),
    trackLoop(Res,T, X), Y is X+1,
    sumLoop(Filtered,Res,Result). 
%sum up two lists elements and produce a new list
sumLoop([],[],[]).
sumLoop([H1|T1],[H2|T2],L3):-
    Y is H1+H2,
    sumLoop(T1,T2,ReTail),
    L3 = [Y|ReTail].
%divide by constant
divideLoop([],_,[]).
divideLoop([Head|Tail], K, Result):-
    Y is Head/K,
    divideLoop(Tail,K,ResultTail),
    Result = [Y|ResultTail]. 
%multiply by constant       
multiplyConstant([],_,[]).
multiplyConstant([Head|Tail], K, Result):-
    Y is Head*K,
    multiplyConstant(Tail,K,ResultTail),
    Result = [Y|ResultTail].
%artistFeatures(+ArtistName, -ArtistFeatures)
artistFeatures(ArtistName, ArtistFeatures):- 
    artist(ArtistName,_,T),albumLoop(Hello, T,X), divideLoop(Hello,X,ArtistFeatures).
albumLoop(Out,[H|[]],Felt):-
    albumFeatures(H,Filter), album(H,_,_,K),
    length(K, Felt), multiplyConstant(Filter, Felt, Out).
albumLoop(Result,[Hem|T],Y):- 
    albumFeatures(Hem,Filtered),
    album(Hem,_,_,P), length(P,Feel), multiplyConstant(Filtered, Feel, And),
    albumLoop(Res,T, X), Y is X + Feel ,
    sumLoop(And,Res,Result).
%multiply the elements of two lists and produce a new one
multiplyLoop([],[],[]).
multiplyLoop([Head|Tail], [H1|T1], Result):-
    Y is Head*H1,
    multiplyLoop(Tail,T1,ResultTail),
    Result = [Y|ResultTail].
%trackDistance(+TrackId1, +TrackId2, -Score)
trackDistance(TrackId1, TrackId2, Score):-
    track(TrackId1,_,_,_,D1), track(TrackId2,_,_,_,D2),
    filter_features(D1,Filtered1),filter_features(D2,Filtered2), distanceFinder(Score, Filtered1,Filtered2).
%distance between two elements
distanceFinder(Result, X,Y):-
    divideLoop(X,-1,Z), sumLoop(Y,Z,T),
    multiplyLoop(T,T,K), sumElementList(K,A), Result is sqrt(A).    
%probably same as in list library
sumElementList([], 0).
sumElementList([Head | Tail], Total) :-
    sumElementList(Tail, Sum1),
    Total = Head + Sum1.

%albumDistance(+AlbumId1, +AlbumId2, -Score)
albumDistance(AlbumId1, AlbumId2, Score):-
    albumFeatures(AlbumId1,Feature1), albumFeatures(AlbumId2,Feature2), distanceFinder(Score,Feature1,Feature2). 
%artistDistance(+ArtistName1, +ArtistName2, -Score)
artistDistance(ArtistName1, ArtistName2, Score):-
    artistFeatures(ArtistName1,Feature1), artistFeatures(ArtistName2,Feature2), distanceFinder(Score,Feature1,Feature2). 

%findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames):-
    findall(X-Y, (trackDistance(TrackId,Y,X),\+(TrackId = Y)), Term), msort(Term,T),
    showIDs(SimilarIds, T,0), showNames(SimilarNames, T,0).
showIDs([],_, 30).
showIDs(Result, [_-M|T], X):-
    Y is X+1, showIDs(Ret, T,Y), Result = [M|Ret]. 
showNames([],_, 30).
showNames(Result, [_-M|T], X):-
    Y is X+1, track(M,B,_,_,_),
    showNames(Ret, T,Y), Result = [B|Ret].

%findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames):-
    findall(X-Y, (albumDistance(AlbumId,Y,X), \+(AlbumId = Y)), Term), msort(Term, T),
    showIDs(SimilarIds, T,0), showAlbumNames(SimilarNames, T,0).
showAlbumNames([],_, 30).
showAlbumNames(Result, [_-M|T], X):-
    Y is X+1, album(M,B,_,_),
    showAlbumNames(Ret, T,Y), Result = [B|Ret].

%findMostSimilarArtists(+ArtistName, -SimilarArtists)
findMostSimilarArtists(ArtistName, SimilarArtists):-findall(X-Y, (artistDistance(ArtistName,Y,X),\+(ArtistName = Y) ), Term),
msort(Term, T), showIDs(SimilarArtists, T,0).


%filterExplicitTracks(+TrackList, -FilteredTracks)
filterExplicitTracks(TrackList, FilteredTracks) :-
    findall(X, (member(X,TrackList), track(X,_,_,_,[0|_])), FilteredTracks).

%getTrackGenre(+TrackId, -Genres)
getTrackGenre(TrackId, Genres):- track(TrackId,_,Artists,_,_), artistLoop(Result, Artists),flatten(Result, Mid), list_to_set(Mid,Genres).
artistLoop([],[]).
artistLoop(Result, [H|T]):-
    artist(H,K,_), artistLoop(Rem, T),
    Result = [K|Rem].

%discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist)
discoverPlaylist(LikedGenres, DislikedGenres, Features,FileName, Playlist):- 
    subGenre(LikedGenres, Other1),subGenre(DislikedGenres, Other2),eliminator(Other1, Other2, Res),
    list_to_set(Res,Res1),distanceLoop(Res1,Features,Final),msort(Final,Final2) ,showAll(Playlist,Points,Final2,0),
    nameAndArtist(Names,Artists,Playlist),printPhase(FileName,Playlist,Names,Artists,Points).
%helper for taking the first 30 elements of a list
showAll([],[],_, 30).
showAll(Result,Res2 ,[N-M|T], X):-
    Y is X+1, showAll(Ret, Ret2, T,Y), Result = [M|Ret], Res2=[N|Ret2].
%helper for print phase
nameAndArtist([],[],[]).
nameAndArtist(Names, Artists, [H|T]):-
    track(H,X,Y,_,_), nameAndArtist(N1,A1,T), Names = [X|N1], Artists = [Y|A1].
%print phase of the last predicate
printPhase(File, Ids, Names,Artists,Points):- open(File, write, Stream),
   writeln(Stream, Ids), writeln(Stream,Names), writeln(Stream,Artists),writeln(Stream,Points), close(Stream).
%eliminates all disliked and liked ones among all tracks
eliminator(Liked,Disliked,Result):-
    findall(X,(
        getTrackGenre(X,Genres),
        (isLiked(Genres,Liked), \+ isLiked(Genres,Disliked))
    ),Result).
%calculates all distances againts a feature set
distanceLoop([],_,[]).
distanceLoop([H|T], Features, Result):- track(H,_,_,_,L), distanceLoop(T,Features, Remi),
 filter_features(L,L1), distanceFinder(Rex, Features, L1), Result = [Rex-H|Remi].   
%controls if an element is included in both lists
isLiked(Genres,Liked):- (member(X,Genres), member(X,Liked)) .
%find all genre types includes substring ones
subGenre([],_).
subGenre(Genres, List):-
    findall(L, artist(_,L,_), WholeGenres), flatten(WholeGenres,Flatten),
    list_to_set(Flatten, Setted),
    findall(X,(member(X,Setted),member(Y,Genres) , sub_string(X,_,_,_,Y)), List).