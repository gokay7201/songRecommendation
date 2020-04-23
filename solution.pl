% mehmet gokay yildiz
% 2017400072
% compiling: yes
% complete: yes

%artist(ArtistName, Genres, AlbumIds).
%album(AlbumId, AlbumName, ArtistNames, TrackIds).
%track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).
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

getAlbumes([],[],_).
getAlbumes(List,[H|T],X):- album(H,_,_,L),
    getTracks(Lisk,L,X),
    getAlbumes(Bebe,T,X),
    List = [Lisk|Bebe].

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

trackLoop(Filter,[H|[]],1):-  track(H,_,_,_,Ved), filter_features(Ved,Filter).
trackLoop(Result,[Hem|T],Y):- 
    track(Hem,_,_,_,Ded),
    filter_features(Ded,Filtered),
    trackLoop(Res,T, X), Y is X+1,
    sumLoop(Filtered,Res,Result). 

sumLoop([],[],[]).
sumLoop([H1|T1],[H2|T2],L3):-
    Y is H1+H2,
    sumLoop(T1,T2,ReTail),
    L3 = [Y|ReTail].

divideLoop([],_,[]).
divideLoop([Head|Tail], K, Result):-
    Y is Head/K,
    divideLoop(Tail,K,ResultTail),
    Result = [Y|ResultTail].    
multiplyConstant([],_,[]).
multiplyConstant([Head|Tail], K, Result):-
    Y is Head*K,
    multiplyConstant(Tail,K,ResultTail),
    Result = [Y|ResultTail].


%artistFeatures(+ArtistName, -ArtistFeatures) 5 points
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



multiplyLoop([],[],[]).
multiplyLoop([Head|Tail], [H1|T1], Result):-
    Y is Head*H1,
    multiplyLoop(Tail,T1,ResultTail),
    Result = [Y|ResultTail].

%trackDistance(+TrackId1, +TrackId2, -Score) 5 points
trackDistance(TrackId1, TrackId2, Score):-
    track(TrackId1,_,_,_,D1), track(TrackId2,_,_,_,D2),
    filter_features(D1,Filtered1),filter_features(D2,Filtered2), distanceFinder(Score, Filtered1,Filtered2).

distanceFinder(Result, X,Y):-
    divideLoop(X,-1,Z), sumLoop(Y,Z,T),
    multiplyLoop(T,T,K), sumElementList(K,A), Result is sqrt(A).    

sumElementList([], 0).
sumElementList([Head | Tail], Total) :-
    sumElementList(Tail, Sum1),
    Total = Head + Sum1.

%albumDistance(+AlbumId1, +AlbumId2, -Score) 5 points
albumDistance(AlbumId1, AlbumId2, Score):-
    albumFeatures(AlbumId1,Feature1), albumFeatures(AlbumId2,Feature2), distanceFinder(Score,Feature1,Feature2). 
%artistDistance(+ArtistName1, +ArtistName2, -Score) 5 points
artistDistance(ArtistName1, ArtistName2, Score):-
    artistFeatures(ArtistName1,Feature1), artistFeatures(ArtistName2,Feature2), distanceFinder(Score,Feature1,Feature2). 


%findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames):-
    findall(X-Y, trackDistance(TrackId,Y,X), Term), sort(0,@=<, Term, [_|T]),
    showIDs(SimilarIds, T,1), showNames(SimilarNames, T,0).
showIDs([],_, 30).
showIDs(Result, [_-M|T], X):-
    Y is X+1, showIDs(Ret, T,Y), Result = [M|Ret]. 
showNames([],_, 30).
showNames(Result, [_-M|T], X):-
    Y is X+1, track(M,B,_,_,_),
    showNames(Ret, T,Y), Result = [B|Ret].

%findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames):-
    findall(X-Y, albumDistance(AlbumId,Y,X), Term), sort(0,@=<, Term, [_|T]),
    showIDs(SimilarIds, T,1), showAlbumNames(SimilarNames, T,0).
showAlbumNames([],_, 30).
showAlbumNames(Result, [_-M|T], X):-
    Y is X+1, album(M,B,_,_),
    showAlbumNames(Ret, T,Y), Result = [B|Ret].

%findMostSimilarArtists(+ArtistName, -SimilarArtists) 10 points
findMostSimilarArtists(ArtistName, SimilarArtists):-findall(X-Y, artistDistance(ArtistName,Y,X), Term),
sort(0,@=<, Term, [_|T]), showIDs(SimilarArtists, T,0).


%filterExplicitTracks(+TrackList, -FilteredTracks) 5 points
filterExplicitTracks(TrackList, FilteredTracks) :-
    findall(X, (member(X,TrackList), track(X,_,_,_,[0|_])), FilteredTracks).

%getTrackGenre(+TrackId, -Genres) 5 points
getTrackGenre(TrackId, Genres):- track(TrackId,_,Artists,_,_), artistLoop(Result, Artists),flatten(Result, Genres).
artistLoop([],[]).
artistLoop(Result, [H|T]):-
    artist(H,K,_), artistLoop(Rem, T),
    Result = [K|Rem].


% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist) 30 points