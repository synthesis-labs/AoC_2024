internal interface IDatasetReader
{
    Task<List<string>> ReadDatasetAsync(int year, int day, string? localFilePath);
}

internal class DatasetReader: IDatasetReader
{
    private readonly string _sessionCookie;

    public DatasetReader(string sessionCookie)
    {
        _sessionCookie = sessionCookie;
    }

    public async Task<List<string>> ReadDatasetAsync(int year, int day, string? localFilePath = null)
    {
        if (localFilePath != null)
        {
            if (File.Exists(localFilePath))
                return new List<string>(File.ReadAllLines(localFilePath));
            else
                throw new FileNotFoundException($"The file {localFilePath} does not exist.");
        }
        else
        {
            string url = $"https://adventofcode.com/{year}/day/{day}/input";
            using (var client = new HttpClient())
            {
                client.DefaultRequestHeaders.Add("Cookie", $"session={_sessionCookie}");
                var response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string content = await response.Content.ReadAsStringAsync();
                return new List<string>(content.Split(['\n', '\r'], StringSplitOptions.RemoveEmptyEntries));
            }
        }
    }
}