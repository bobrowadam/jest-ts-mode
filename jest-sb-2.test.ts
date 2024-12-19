describe("[prepareBloombergForwardPointsConfig]", () => {
  test("it should return bloomberg forward points tickers config with a default convention if a custom ticker is not passed", async () => {
    const params = {
      fromCurrency: "AUD",
      toCurrency: "CAD",
      ticker: null,
      queryRunner: AppDataSource.createQueryRunner(),
    };
    // @ts-expect-error - moving to strict ts
    const config = await prepareBloombergForwardPointsConfig(params);
    expect(config).toMatchObject({
      "7": "AUDCAD1W BGN Curncy",
      "14": "AUDCAD2W BGN Curncy",
      "30": "AUDCAD1M BGN Curncy",
      "60": "AUDCAD2M BGN Curncy",
      "90": "AUDCAD3M BGN Curncy",
      "120": "AUDCAD4M BGN Curncy",
      "150": "AUDCAD5M BGN Curncy",
      "180": "AUDCAD6M BGN Curncy",
      "210": "AUDCAD7M BGN Curncy",
      "240": "AUDCAD8M BGN Curncy",
      "270": "AUDCAD9M BGN Curncy",
      "300": "AUDCAD10M BGN Curncy",
      "330": "AUDCAD11M BGN Curncy",
      "360": "AUDCAD12M BGN Curncy",
    });
  });

  test("it should return bloomberg forward points tickers config with a customer ticker if passed and a correlated spot ticker", async () => {
    const params = {
      fromCurrency: "CNY",
      toCurrency: "EUR",
      ticker: "CCEUN+",
      queryRunner: AppDataSource.createQueryRunner(),
    };
    const config = await prepareBloombergForwardPointsConfig(params);
    expect(config).toMatchObject({
      "0": "EURCNY Curncy",
      "7": "CCEUN+1W BGN Curncy",
      "14": "CCEUN+2W BGN Curncy",
      "30": "CCEUN+1M BGN Curncy",
      "60": "CCEUN+2M BGN Curncy",
      "90": "CCEUN+3M BGN Curncy",
      "120": "CCEUN+4M BGN Curncy",
      "150": "CCEUN+5M BGN Curncy",
      "180": "CCEUN+6M BGN Curncy",
      "210": "CCEUN+7M BGN Curncy",
      "240": "CCEUN+8M BGN Curncy",
      "270": "CCEUN+9M BGN Curncy",
      "300": "CCEUN+10M BGN Curncy",
      "330": "CCEUN+11M BGN Curncy",
      "360": "CCEUN+12M BGN Curncy",
    });
  });
});
