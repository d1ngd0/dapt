SELECT
	count(*) as "count",
	mean("~.http.response_time") as mean_time
FROM "logs"
WHERE
	"http.path" == "/api/v1/organizations" AND
	"~http.response_time" >= 100
HAVING "mean_time" > 100
GROUP BY "host.name"
ORDER BY "count" DESC
INTERVAL 1m
