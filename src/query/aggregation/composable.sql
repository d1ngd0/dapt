SELECT avg(grade) as "avg"
SELECT count(grade) as "avg_count", sum(grade) as "avg_sum"
SELECT div(sum(grade), sum(avg_count)) as "avg"
