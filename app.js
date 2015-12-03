(function() {
    var app = angular.module('elam03.github.io.app', ['ui.bootstrap', 'ngAnimate']);

    app.controller('ProjectListController', function($scope, $http) {
        $scope.oneAtATime = true;

        var preview_path = '1gam_projects';

        $http.get(preview_path + '/project_list.json').then(function(res) {
            $scope.groups = res.data;
            // $scope.groups.previews[0] = 'blar';

            for (var i = 0; i < $scope.groups.length; ++i)
            {
                for (var j = 0; j < $scope.groups[i].previews.length; ++j)
                {
                    var p = $scope.groups[i].previews[j];

                    // console.log('preview: ' + p);

                    $scope.groups[i].previews[j] = preview_path + '/' + p;
                }
            }
        });
    });

    app.controller('DropdownCtrl', function($scope) {
        $scope.items = [
            "The first choice!",
            "And another choice for you.",
            "but wait! A third!"
        ];
    });

    app.controller('TabsController', function($scope, $window) {
        $scope.tabs = [
            { title:'Interests', content:'My interests...' },
            // { title:'Dynamic Title 2', content:'Dynamic content 2', disabled: true }
        ];

        $scope.alertMe = function() {
            setTimeout(function() {
                $window.alert('You\'ve selected the alert tab!');
            });
        };

        $scope.tabs[0].active = true;
    });

    app.controller('CarouselDemoCtrl', function ($scope) {
        $scope.myInterval = 5000;
        $scope.noWrapSlides = false;
        var slides = $scope.slides = [];
        $scope.addSlide = function() {
        var newWidth = 600 + slides.length + 1;
            slides.push({
              image: '//placekitten.com/' + newWidth + '/300',
              text: ['More','Extra','Lots of','Surplus'][slides.length % 4] + ' ' +
                    ['Cats', 'Kittys', 'Felines', 'Cutes'][slides.length % 4]
            });
        };
        for (var i=0; i<4; i++) {
            $scope.addSlide();
        }
    });
})();
