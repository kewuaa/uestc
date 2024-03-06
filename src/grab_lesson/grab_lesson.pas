program grab_lesson;

{$Mode objfpc}{$H+}

uses
    sysutils, classes,
    simpleinternet, internetaccess,
    TOML,
    uestc;

const
    URL = 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/dxkc';
var
    i, j: Int32;
    client: TInternetAccess;
    username, password: String;
    candidates: TStringList;
    v: TTOMLData;
    res: String;
    id, number, name: String;
    trs, tr: IXQValue;
    stop: Boolean = False;
    f: TStringStream;
    config: TTOMLDocument;
begin
    try
        if FileExists('./config.toml') then
        begin
            f := TStringStream.Create();
            f.LoadFromFile('./config.toml');
            config := GetTOML(f.DataString);
            f.Free();
        end
        else
        begin
            raise Exception.Create('not found config.toml in current work directory');
        end;
        client := defaultInternet();
        client.additionalHeaders.AddPair('Accept', 'application/json, text/javascript, */*; q=0.01');
        client.additionalHeaders.AddPair('User-Agent', 'Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0');
        Assert(
            config.Contains('username')
            and config.Contains('password')
            and config.Contains('candidates'),
            'make sure that "username", "password" and "candidates" fields in config.toml'
        );
        username := config['username'].ToString();
        password := config['password'].ToString();
        candidates := TStringList.Create();
        for v in config['candidates'] as TTOMLArray do
        begin
            candidates.Add(v.ToString());
        end;
        eportal_login(client, username, password);
        WriteLn('login successfully');
        visit_graduate_system(client);
        repeat
            WriteLn('***************************************************************************************************');
            res := client.get(URL);
            trs := process(res, '//table[1]//tr');
            for i := 1 to trs.Count - 1 do
            begin
                tr := trs.get(i);
                if tr.map('./td[12]/a').isAssigned() then
                begin
                    id := tr.map('./td[1]/text()').toString();
                    number := tr.map('./td[2]/text()').toString();
                    name := tr.map('./td[3]/text()').toString();
                    WriteLn('<' + name + '> 可选');
                    if candidates.IndexOf(number) <> -1 then
                    begin
                        WriteLn('<' + name + '> 匹配');
                        WriteLn('尝试选取');
                        try
                            select_class(client, id);
                        except
                            on E: SelectFailedError do
                            begin
                                WriteLn('选取失败：' + E.ToString());
                                Continue;
                            end;
                        end;
                        WriteLn('选取成功');
                    end
                    else
                    begin
                        WriteLn('<' + name + '> 不匹配');
                    end;
                end;
            end;
            WriteLn('***************************************************************************************************');
            Sleep(1000);
        until stop;
    finally
        FreeAndNil(client);
        FreeAndNil(config);
        FreeAndNil(v);
        FreeAndNil(candidates);
        FreeAndNil(trs);
        FreeAndNil(tr);
        FreeAndNil(f);
    end;
end.
