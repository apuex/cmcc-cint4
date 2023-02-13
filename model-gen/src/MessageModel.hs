{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
module MessageModel where

import qualified Metadata               as Meta
import qualified Data.List              as L
import qualified Data.Text              as T

-- Enums
enumRightLevel = Meta.EnumerateField
    { Meta.fieldName    = "RightLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "INVALID"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "无权限"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "具备数据读的权限,当用户可以读某个数据，而无法写任何数据时返回这一权限值。"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "具备数据读、写的权限，当用户对某个数据具有读写权限时返回这一权限值。"
                        }
        ]
    , Meta.fieldComment = "监控系统下级SC向上级SC提供的权限定义"
    }


-- Fields
userName = Meta.StringField
    { Meta.fieldName    = "UserName"
    , Meta.fieldSize    = Just 20
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login user name."
    }

passWord = Meta.StringField
    { Meta.fieldName    = "PassWord"
    , Meta.fieldSize    = Just 20
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login password."
    }

-- Message header and CRC16 fields

msgHeader = Meta.Int32Field
    { Meta.fieldName    = "Header"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = "0x7E7C6B5A"
    , Meta.fieldComment = "Message header, 0x7E7C6B5A."
    }

msgLength = Meta.Int32Field
    { Meta.fieldName    = "Length"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message length."
    }

msgSerialNo = Meta.Int32Field
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message serial number."
    }

msgPKType = Meta.Int32Field
    { Meta.fieldName    = "PKType"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "PKType."
    }

msgCRC16 = Meta.UInt16Field
    { Meta.fieldName    = "CRC16"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message CRC16."
    }

-- messages
heartBeat = Meta.Message
    { Meta.entityId      = "Message::HEART_BEAT"
    , Meta.entityName   = "HeartBeat"
    , Meta.entityFields = []
    }

login = Meta.Message
    { Meta.entityId      = "Message::LOGIN"
    , Meta.entityName   = "Login"
    , Meta.entityFields =
        [ userName
        , passWord
        ]
    }

loginAck = Meta.Message
    { Meta.entityId      = "Message::LOGIN_ACK"
    , Meta.entityName   = "LoginAck"
    , Meta.entityFields =
        [ enumRightLevel
        ]
    }

logout = Meta.Message
    { Meta.entityId      = "Message::LOGOUT"
    , Meta.entityName   = "Logout"
    , Meta.entityFields = []
    }

logoutAck = Meta.Message
    { Meta.entityId      = "Message::LOGOUT_ACK"
    , Meta.entityName   = "LogoutAck"
    , Meta.entityFields = []
    }


model = Meta.Model
    { Meta.namespace = "message"
    , Meta.headerFields = 
        [ msgHeader
        , msgLength
        , msgSerialNo
        , msgPKType
        , msgCRC16
        ]
    , Meta.entities =
        [ heartBeat
        , login, loginAck
        , logout, logoutAck
        ]
    , Meta.enumerates =
        [ enumRightLevel
        ]
    }

