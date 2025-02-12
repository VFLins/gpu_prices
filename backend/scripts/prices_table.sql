SELECT
    d.Id,
    d.ProductId,
    c.NameId,
    c.ProductName,
    c.ProductBrand,
    c.ProductModel,
    c.ProductFilters,
    c.Created,
    c.LastUpdate,
    d.Date,
    d.Price,
    d.Name,
    d.Store,
    d.Url
FROM (
    SELECT
        b.Id,
        b.NameId,
        a.ProductName,
        b.ProductBrand,
        b.ProductModel,
        b.ProductFilters,
        b.Created,
        b.LastUpdate
    FROM product_names a
    JOIN products b ON a.Id = b.NameId
) c
JOIN prices d ON c.Id = d.ProductId;
