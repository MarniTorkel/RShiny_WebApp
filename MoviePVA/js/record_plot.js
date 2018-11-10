(function($) {
    $(document).ready(function() {
	
	$('#record_plot').scianimator({
	    'images': ['images/record_plot1.png', 'images/record_plot2.png', 'images/record_plot3.png', 'images/record_plot4.png', 'images/record_plot5.png', 'images/record_plot6.png', 'images/record_plot7.png', 'images/record_plot8.png', 'images/record_plot9.png', 'images/record_plot10.png', 'images/record_plot11.png'],
	    'width': 480,
	    'delay': 1000,
	    'loopMode': 'loop'
	});
	$('#record_plot').scianimator('play');
    });
})(jQuery);
