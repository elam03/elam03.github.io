(function() {
    var app = angular.module('collection', []);

    app.controller('MovieDataLoaderController', function($scope, $http) {
        $http.get('movies.json').then(function(res) {
          $scope.items = res.data;
        });
    });

    app.directive('itemTitle', function() {
        return {
            restrict: 'E',
            templateUrl: 'templates/item-title.html'
        };
    });

    app.directive('itemPanels', function() {
        return {
            restrict: 'E',
            templateUrl: 'templates/item-panels.html',
            controller: function() {
                this.tab = 1;

                this.selectTab = function(setTab) {
                    this.tab = setTab;
                };

                this.isTabSelected = function(checkTab) {
                    return this.tab === checkTab;
                };
            },
            controllerAs: 'panel'
        };
    });

    app.controller('UserController', function($scope){
        $scope.user = user;
    });

    app.controller('CollectionController', function($http) {
        this.items = movies;
    });

    var user = {
        "name": "Eric Lam",
        "status": "yay",
        "number": "10"
    };

    var guest_user = {
        name: 'Guest User',
        status: 'nay?',
        number: '2'
    };

    var movie = {
        name: "Sound of Music",
        gross: 163.2,
        release: new Date('1965-03-29'),
        inCollection: true,
        shortDescription: 'A woman leaves an Austrian convent to become a governess to the children of a Naval officer Widower.',
        guiltyPleasure: false
    };

    var movies = [
        {
            name: "Sound of Music",
            gross: 163.2,
            release: new Date('1965-03-29'),
            inCollection: true,
            shortDescription: 'A woman leaves an Austrian convent to become a governess to the children of a Naval officer Widower.',
            guiltyPleasure: false
        },
        {
            name: "Mary Poppins",
            gross: 102.3,
            release: new Date('1964-09-11'),
            inCollection: false,
            shortDescription: "A magic nanny comes to work for a cold banker's unhappy family",
            guiltyPleasure: false
        }
    ];
})();
