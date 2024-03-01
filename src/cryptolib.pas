unit cryptolib;

{$Mode objfpc}{$H+}

interface

function aes_encrypt(plaintext, key: String): String;

implementation

uses
    base64;

{$LinkLib crypto}
function EVP_aes_128_cbc: Pointer; cdecl; external;
function EVP_CIPHER_CTX_new: Pointer; cdecl; external;
procedure EVP_CIPHER_CTX_free(ctx: Pointer); cdecl; external;
function EVP_EncryptInit_ex(ctx: Pointer; const type_: Pointer; impl: Pointer; key: PByte; iv: PByte): Integer; cdecl; external;
function EVP_CIPHER_CTX_set_padding(ctx: Pointer; padding: Integer): Integer; cdecl; external;
function EVP_EncryptUpdate(ctx: Pointer; data_out: PByte; out_len: PInteger; data_in: PByte; in_len: Integer): Integer; cdecl; external;
function EVP_EncryptFinal_ex(ctx: Pointer; data_out: PByte; out_len: PInteger): Integer; cdecl; external;

function aes_encrypt(plaintext, key: String): String;
const
    IV = 'fBB8WZr28H3t7NGt';
    PAD = 'KbN6ApjTFGswB8x3AdKPJ6NKMHJsbfZQ76cXGYdkhcGjxcjnQhXAAE2rrMndEheW';
var
    ctx: Pointer;
    len: Int32;
    ciphertext: String;
    ciphertext_len: Int32;
    ptr: PChar;
begin
    // 初始化 OpenSSL 加密上下文
    ctx := EVP_CIPHER_CTX_new();
    plaintext := PAD + plaintext;

    try
        // 设置 AES-128-CBC 加密算法
        EVP_EncryptInit_ex(ctx, EVP_aes_128_cbc(), nil, @key[1], @IV[1]);

        // 设置填充模式为 PKCS7
        EVP_CIPHER_CTX_set_padding(ctx, 1);

        // 加密数据
        SetLength(ciphertext, 100); // Make sure there is enough space for padded output

        // 加密
        EVP_EncryptUpdate(ctx, @ciphertext[1], @len, @plaintext[1], Length(plaintext));
        ciphertext_len := len;

        // 最终加密
        EVP_EncryptFinal_ex(ctx, @ciphertext[len + 1], @len);
        ciphertext_len := ciphertext_len + len;
        SetLength(ciphertext, ciphertext_len);
        Result := EncodeStringBase64(ciphertext);
    finally
        EVP_CIPHER_CTX_free(ctx);
    end;
end;

end.
