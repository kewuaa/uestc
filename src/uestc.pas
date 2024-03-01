unit uestc;

{$Mode objfpc}{$H+}

interface

uses
    sysutils, classes,
    internetaccess;

type
    AuthorizeError = class(Exception);

    SelectFailedError = class(Exception);

    TLessonInfo = record
        id: String;
        name, department, teacher, time_place, location: UnicodeString;
        hour, credit, capacity, size, weight: Int32;
        operable: Boolean;
    end;
    TLessonArray = array of TLessonInfo;

procedure eportal_login(const client: TInternetAccess; const username, password: String);
function check_login(const client: TInternetAccess): Boolean;
procedure visit_graduate_system(const client: TInternetAccess);
function search_lesson(const client: TInternetAccess; const lesson_id: String): String;
procedure update_class_weight(const client: TInternetAccess; const class_id: String; new_weight: Int32);
procedure select_class(const client: TInternetAccess; const class_id: String; weight: Int32 = -1);
function fetch_class_table(const client: TInternetAccess): String;

implementation

uses
    dateutils, fpjson, jsonparser,
    base64, fpimage, fpreadjpeg, fpreadpng,
    simpleinternet, xquery,
    cryptolib;

type
    TImageArray = array of Double;
    TSliderData = class
    private
        _fg, _bg: TFPMemoryImage;
    public
        constructor Create(const slider_res: RawByteString);
        destructor Destroy(); override;
        function get_move_length(): Double;
    end;

function get_timestamp(): Int64;
begin
    Result := MilliSecondsBetween(
        Now,
        EncodeDateTime(1970, 1, 1, 0, 0, 0, 0)
    );
end;

generic function load_image_from_b64str<T: TFPCustomImageReader>(
    const b64str: String
): TFPMemoryImage;
var
    reader: T;
    buf: TMemoryStream;
    img: TFPMemoryImage;
    str: String;
begin
    buf := Nil;
    try
        img := TFPMemoryImage.Create(0, 0);
        reader := T.Create();
        buf := TMemoryStream.Create();
        str := DecodeStringBase64(b64str);
        buf.Write(str[1], str.Length);
        buf.Position := 0;
        img.LoadFromStream(buf, reader);
        Result := img;
    finally
        FreeAndNil(buf);
    end;
end;

function turn_gray(const img: TFPMemoryImage; byte_per_pixel: SizeUInt): TImageArray;
var
    row, col: Int32;
    pixel: TFPColor;
    arr: TImageArray;
begin
    SetLength(arr, img.Width * img.Height);
    for row := 0 to img.Height - 1 do
    begin
        for col := 0 to img.Width - 1 do
        begin
            pixel := img.Colors[col, row];
            arr[row * img.Width + col] := (pixel.Red / 256 * 299 + pixel.Green / 256 * 587 + pixel.Blue / 256 * 114) / 1000;
        end;
    end;
    Result := arr;
end;

{$LinkLib verify}
function calculate_move_length(
    fg_width: Int32;
    bg_width: Int32;
    height: Int32;
    fg_data: PDouble;
    bg_data: PDouble
): Double; cdecl; external;

constructor TSliderData.Create(const slider_res: RawByteString);
var
    fg_str, bg_str: String;
    json_data: TJSONObject;
begin
    try
        json_data := GetJSON(slider_res) as TJSONObject;
        fg_str := json_data.Get('smallImage', '');
        bg_str := json_data.Get('bigImage', '');
    finally
        FreeAndNil(json_data);
    end;
    Assert(fg_str <> '', 'Failed to get small_image');
    Assert(bg_str <> '', 'Failed to get big_image');
    self._fg := specialize load_image_from_b64str<TFPReaderPNG>(fg_str);
    self._bg := specialize load_image_from_b64str<TFPReaderJPEG>(bg_str);
    Assert(self._fg.Height = self._fg.Height, 'height not same');
end;

function TSliderData.get_move_length(): Double;
var
    fg_data, bg_data: TImageArray;
begin
    try
        fg_data := turn_gray(self._fg, 4);
        bg_data := turn_gray(self._bg, 3);
        Result := calculate_move_length(
            self._fg.Width,
            self._bg.Width,
            self._fg.Height,
            @fg_data[0],
            @bg_data[0]
        );
    finally
        SetLength(fg_data, 0);
        SetLength(bg_data, 0);
    end;
end;

destructor TSliderData.Destroy();
begin
    FreeAndNil(self._fg);
    FreeAndNil(self._bg);
end;

procedure eportal_login(const client: TInternetAccess; const username, password: String);
const
    login_page_url = 'https://idas.uestc.edu.cn/authserver/login?'
        + 'service=https%3A%2F%2Feportal.uestc.edu.cn%3A443%2Flogin%3Fservice%3Dhttps%3A%2F%2Feportal.uestc.edu.cn%2Fnew%2Findex.html%3Fbrowser%3Dno';
var
    res: String;
    hidden_value: TStringArray;
    lt, dllt, execution, _eventId, rmShown, key: String;
    slider_data: TSliderData;
    move_length: Int32;
    json: TJSONObject;
    sign: String;
    data: TStringList;
begin
    try
        res := client.get(login_page_url);
        hidden_value := process(res, '//form[@id="casLoginForm"]//input[@type="hidden"]/@value').toStringArray();
        lt := hidden_value[0];
        dllt := hidden_value[1];
        execution := hidden_value[2];
        _eventId := hidden_value[3];
        rmShown := hidden_value[4];
        key := hidden_value[5];
        SetLength(hidden_value, 0);

        res := client.get(
            'https://idas.uestc.edu.cn/authserver/sliderCaptcha.do?_=' + IntToStr(get_timestamp())
        );
        slider_data := TSliderData.Create(res);
        move_length := Round(slider_data.get_move_length() * 280);
        res := client.get(
            'https://idas.uestc.edu.cn/authserver/verifySliderImageCode.do?canvasLength=280&moveLength=' + IntToStr(move_length)
        );
        json := GetJSON(res) as TJSONObject;
        if json.Get('code', 1) <> 0 then
        begin
            raise Exception.Create('verify failed: ' + json.Get('message', ''));
        end;
        sign := json.Get('sign', '');
        Assert(sign <> '', 'empty sign');

        data := TStringList.Create();
        data.AddPair('username', username);
        data.AddPair('password', aes_encrypt(password, key));
        data.AddPair('lt', lt);
        data.AddPair('dllt', dllt);
        data.AddPair('execution', execution);
        data.AddPair('_eventId', _eventId);
        data.AddPair('rmShown', rmShown);
        data.AddPair('sign', sign);
        res := client.post(login_page_url, TInternetAccess.urlEncodeData(data));
        if res.Contains('class="auth_error"') then
        begin
            raise AuthorizeError.Create('Incorrect username or password');
        end;
    finally
        FreeAndNil(data);
        FreeAndNil(json);
        FreeAndNil(slider_data);
    end;
end;

function check_login(const client: TInternetAccess): Boolean;
var
    url: String;
    res: String;
    json: TJSONObject;
begin
    try
        url := 'https://eportal.uestc.edu.cn/jsonp/userDesktopInfo.json?type=&_=' + IntToStr(get_timestamp());
        res := client.get(url);
        json := GetJSON(res, False) as TJSONObject;
        WriteLn(json.Get('userName', ''), ' ', json.Get('userDepartment', ''));
        Result := json.Get('hasLogin', False);
    finally
        FreeAndNil(json);
    end;
end;

procedure visit_graduate_system(const client: TInternetAccess);
const
    URL = 'https://eportal.uestc.edu.cn/appShow?appId=5609306976424512';
begin
    client.get(URL);
end;

function fetch_lessons(const client: TInternetAccess): TLessonArray;
const
    URL = 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/dxkc';
    // URL := 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/yxkc';
var
    res: String;
    i: Int32;
    trs, tds: IXQValue;
    lessons: TLessonArray;
begin
    res := client.get(URL);
    trs := process(res, '//table[1]//tr');
    SetLength(lessons, trs.Count);
    for i := 1 to trs.Count - 1 do
    begin
        tds := trs.get(i).map('./td');
        with lessons[0] do
        begin
            id := tds.get(0).toString();
            name := tds.get(1).toString();
            department := tds.get(2).toString();
            teacher := tds.get(3).toString();
            time_place := tds.get(4).toString();
            location := tds.get(5).toString();
            hour := tds.get(6).toInt64();
            credit := tds.get(7).toInt64();
            capacity := tds.get(8).toInt64();
            size := tds.get(9).toInt64();
            weight := tds.get(10).toInt64();
        end;
    end;
    Result := lessons;
end;

function fetch_class_table(const client: TInternetAccess): String;
const
    URL = 'https://yjsjy.uestc.edu.cn/pyxx/pygl/xskbcx/index/1';
var
    res: String;
begin
    res := client.get(URL);
    Result := process(res, '//table[1]').toString();
end;

function search_lesson(
    const client: TInternetAccess;
    const lesson_id: String
): String;
const
    URL = 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/getxkrs';
var
    data: TStringList;
begin
    try
        data := TStringList.Create();
        data.AddPair('kcbh', lesson_id);
        data.AddPair('lcid', '4086795');
        Result := client.post(URL, TInternetAccess.urlEncodeData(data));
    finally
        FreeAndNil(data);
    end;
end;

procedure update_class_weight(
    const client: TInternetAccess;
    const class_id: String;
    new_weight: Int32
);
var
    url: String;
    res: String;
    json: TJSONObject;
begin
    try
        if new_weight > 100 then
        begin
            raise Exception.Create('Invaild weight, must between 0-100');
        end;
        url := 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/xgqz/' + class_id + '/' + IntToStr(new_weight);
        res := client.post(url, '');
        json := GetJSON(res, False) as TJSONObject;
        if json.Get('status', '1') <> '1' then
        begin
            raise Exception.Create('Update weight failed: ' + json.Get('msg', ''));
        end;
    finally
        FreeAndNil(json);
    end;
end;

procedure select_class(
    const client: TInternetAccess;
    const class_id: String;
    weight: Int32 = -1
);
const
    URL = 'https://yjsjy.uestc.edu.cn/pyxx/pygl/pyjhxk/jhnxk';
var
    res: String;
    data: TStringList;
begin
    try
        data := TStringList.Create();
        data.AddPair('bjid', class_id);
        if weight < 0 then
        begin
            data.AddPair('qz', '');
        end
        else if weight > 100 then
        begin
            raise SelectFailedError.Create('Invaild weight, must between 0-100');
        end
        else
        begin
            data.AddPair('qz', IntToStr(weight));
        end;
        res := client.post(URL, TInternetAccess.urlEncodeData(data));
        if res <> '""' then
        begin
            raise SelectFailedError.Create(res);
        end;
    finally
        data.Free();
    end;
end;

end.
