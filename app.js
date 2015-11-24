(function() {
    var app = angular.module('elam03.github.io.app', ['ui.bootstrap']);

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

                    console.log('preview: ' + p);

                    $scope.groups[i].previews[j] = preview_path + '/' + p;
                }
            }
        });
    });
})();
