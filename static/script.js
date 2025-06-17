document.addEventListener('DOMContentLoaded', function() {
    // Add click handlers to all clickable rows
    const clickableRows = document.querySelectorAll('.clickable-row');
    
    clickableRows.forEach(function(row) {
        row.addEventListener('click', function() {
            const targetId = this.getAttribute('data-target');
            const detailRow = document.getElementById(targetId);
            
            if (detailRow) {
                if (detailRow.style.display === 'none' || detailRow.style.display === '') {
                    detailRow.style.display = 'table-row';
                } else {
                    detailRow.style.display = 'none';
                }
            }
        });
    });
});